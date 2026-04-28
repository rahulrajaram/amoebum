#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
CONTINUATION_FILE="${REPO_ROOT}/.yarli/continuation.json"
TRANCHES_FILE="${REPO_ROOT}/.yarli/tranches.toml"
PROMPT_FILE="${REPO_ROOT}/PROMPT.md"
PLAN_FILE="${REPO_ROOT}/IMPLEMENTATION_PLAN.md"
POSTMORTEM_DIR="${REPO_ROOT}/.agent/token-postmortems"
INDEX_FILE="${POSTMORTEM_DIR}/INDEX.md"
OVERRUN_RATIO_THRESHOLD=2
ADVISORY_MAX_TOKENS=100000

usage() {
  cat <<'EOF'
Usage:
  bin/yarli-token-postmortem.sh

Scans .yarli/continuation.json plus PROMPT.md and IMPLEMENTATION_PLAN.md for
recorded per-tranche token usage. For any tranche that consumed more than
2x its declared max_tokens (or, if no per-tranche budget is declared, more
than the 100000-token advisory maximum), writes a structured markdown
postmortem under .agent/token-postmortems/I<tranche>.md and refreshes an
INDEX.md summary.

The script is idempotent: re-running overwrites existing postmortems with
the latest data and rewrites the index. Exits non-zero only on missing
dependencies or unparseable input.

Final line: YARLI_TOKEN_POSTMORTEM_OK count=<N> threshold_ratio=2.0
EOF
}

fail() {
  printf 'YARLI_TOKEN_POSTMORTEM_ERROR: %s\n' "$*" >&2
  exit 1
}

require_cmd() {
  command -v "$1" >/dev/null 2>&1 || fail "missing required command: $1"
}

if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
  usage
  exit 0
fi

require_cmd jq
require_cmd awk
require_cmd rg

[[ -f "${CONTINUATION_FILE}" ]] || fail "missing ${CONTINUATION_FILE}"
[[ -f "${TRANCHES_FILE}" ]] || fail "missing ${TRANCHES_FILE}"

mkdir -p "${POSTMORTEM_DIR}"

# Look up max_tokens for a given tranche key from tranches.toml.
# Prints the integer, or 0 if no max_tokens is declared.
lookup_max_tokens() {
  local key="$1"
  awk -v want_key="${key}" '
    /^\[\[tranches\]\]/ {
      if (in_tranche && cur_key == want_key) {
        print cur_max
        found = 1
        exit
      }
      cur_key = ""
      cur_max = 0
      in_tranche = 1
      next
    }
    in_tranche && /^key = / {
      gsub(/^key = "|"$/, "", $0)
      cur_key = $0
    }
    in_tranche && /^max_tokens = / {
      gsub(/^max_tokens = /, "", $0)
      cur_max = $0 + 0
    }
    END {
      if (!found && in_tranche && cur_key == want_key) {
        print cur_max
      }
    }
  ' "${TRANCHES_FILE}"
}

# Extract documented (tranche_key, total_tokens) pairs from PROMPT.md and
# IMPLEMENTATION_PLAN.md. The patterns are stable phrasings introduced by
# prior status updates: "NXT-### consumed <N> tokens" and "NXT-###=<N>".
extract_documented_records() {
  # First pass: pull lines containing "consumed N tokens" with optional
  # backticks, and the original line for context. Then pair each numeric
  # match with the closest preceding NXT-### key on the same line.
  rg -No 'NXT-[0-9]+|consumed [`]?[0-9]+[`]? tokens' "${PROMPT_FILE}" "${PLAN_FILE}" 2>/dev/null \
    | awk -F: '
      {
        # Recombine the file:line:match if rg printed line numbers.
        match_text = $NF
        # Determine match type.
        if (match_text ~ /^NXT-/) {
          last_key = match_text
        } else if (match_text ~ /^consumed /) {
          n = match_text
          gsub(/[^0-9]/, "", n)
          if (last_key != "" && n != "") {
            tokens = n + 0
            if (tokens > best[last_key]) {
              best[last_key] = tokens
            }
          }
        }
      }
      END {
        for (key in best) {
          printf "%s|%d\n", key, best[key]
        }
      }
    ' | sort
}

# Extract current-run records from continuation.json.
extract_current_records() {
  jq -r '
    (.tranche_token_usage // [])[]
    | select(.tranche_key != null)
    | "\(.tranche_key)|\(.total_tokens // 0)|\(.prompt_tokens // 0)|\(.completion_tokens // 0)"
  ' "${CONTINUATION_FILE}"
}

# Merge documented + current records, preferring current-run rich data.
collect_all_records() {
  local current documented
  current="$(extract_current_records)"
  documented="$(extract_documented_records)"

  # Build a unified list: key|total|prompt|completion|source
  {
    while IFS='|' read -r key total prompt completion; do
      [[ -z "${key}" ]] && continue
      printf '%s|%s|%s|%s|continuation.json\n' "${key}" "${total}" "${prompt}" "${completion}"
    done <<<"${current}"

    while IFS='|' read -r key total; do
      [[ -z "${key}" ]] && continue
      printf '%s|%s|0|0|documented\n' "${key}" "${total}"
    done <<<"${documented}"
  } | awk -F'|' '
    NF >= 5 {
      key = $1
      total = $2 + 0
      # Prefer continuation.json source when both are present for the same key.
      if (!(key in seen) || ($5 == "continuation.json" && source[key] != "continuation.json")) {
        seen[key] = 1
        totals[key] = total
        prompts[key] = $3
        completions[key] = $4
        source[key] = $5
      }
    }
    END {
      for (key in seen) {
        printf "%s|%d|%d|%d|%s\n", key, totals[key], prompts[key], completions[key], source[key]
      }
    }
  ' | sort
}

write_postmortem() {
  local key="$1"
  local total="$2"
  local prompt_t="$3"
  local completion_t="$4"
  local source="$5"
  local max_tokens="$6"
  local ratio="$7"
  local effective_budget="$8"
  local out_file="${POSTMORTEM_DIR}/I${key}.md"
  local generated_at
  generated_at="$(date -u +"%Y-%m-%d")"

  local budget_field
  if [[ "${max_tokens}" -gt 0 ]]; then
    budget_field="declared max_tokens = ${max_tokens}"
  else
    budget_field="no per-tranche max_tokens; advisory max = ${ADVISORY_MAX_TOKENS}"
  fi

  cat >"${out_file}" <<EOF
+++
schema_version = 1
tranche = "${key}"
task_type = "postmortem"
status = "overrun"
summary = "Tranche-run consumed ${total} tokens, ${ratio}x the effective budget of ${effective_budget}."
generated_at = "${generated_at}"
total_tokens = ${total}
prompt_tokens = ${prompt_t}
completion_tokens = ${completion_t}
declared_max_tokens = ${max_tokens}
effective_budget_tokens = ${effective_budget}
overrun_ratio = "${ratio}"
source = "${source}"
+++
# Token-Budget Postmortem: ${key}

## Summary
- Recorded total tokens: \`${total}\`
- Effective budget: \`${effective_budget}\` (${budget_field})
- Overrun ratio: \`${ratio}x\`
- Source: \`${source}\`

## Contributing Factors
- Per-tranche \`max_tokens\` is currently advisory: yarli does not abort a
  worker mid-run when consumption crosses the declared budget, so the worker
  ran to completion despite the overrun.
- The \`bin/yarli-run-risk-audit.sh\` hard-stop fires only on the *next*
  fresh launch attempt after two consecutive >200000-token tranches.
- Recorded breakdown: \`prompt_tokens=${prompt_t}\` /
  \`completion_tokens=${completion_t}\` (zero values indicate the record was
  recovered from documented prose rather than from \`continuation.json\`).

## Remediation Options
1. Reduce the tranche's declared scope: tighten \`allowed_paths\`, narrow
   \`done_when\`, or split the tranche into a paired preferred + fallback
   path before relaunching.
2. Lower the declared \`max_tokens\` so future audits classify the tranche
   accurately without normalizing the overrun.
3. If the work landed cleanly despite the overrun, accept the record but
   require explicit operator review (hard-stop) before launching the next
   tranche of similar shape.

## Related Records
- See \`.agent/yarli-run-risk-audit.md\` for the active hard-stop pair.
- See \`.yarli/evidence/I${key}.md\` if implementation evidence was recorded.
EOF
  printf '%s\n' "${out_file}"
}

write_index() {
  local count="$1"
  local rows="$2"
  local generated_at
  generated_at="$(date -u +"%Y-%m-%dT%H:%M:%SZ")"

  cat >"${INDEX_FILE}" <<EOF
# Token-Budget Postmortem Index

Generated: \`${generated_at}\`
Threshold: any recorded tranche-run with \`total_tokens >= ${OVERRUN_RATIO_THRESHOLD}x\` its effective budget.
Effective budget: declared \`max_tokens\` from \`.yarli/tranches.toml\`, or the \`${ADVISORY_MAX_TOKENS}\`-token advisory if none is declared.

## Postmortems (${count})

| Tranche | Total Tokens | Effective Budget | Ratio | Source |
| --- | ---: | ---: | ---: | --- |
${rows}

## Notes
- This index and the per-tranche \`I<key>.md\` files are regenerated
  idempotently by \`bin/yarli-token-postmortem.sh\`.
- \`.agent/yarli-run-risk-audit.md\` references this index when the
  hard-stop fires.
EOF
}

main() {
  local rows=""
  local count=0
  local records
  records="$(collect_all_records)"

  if [[ -z "${records}" ]]; then
    write_index 0 ""
    printf 'YARLI_TOKEN_POSTMORTEM_OK count=0 threshold_ratio=%s.0\n' "${OVERRUN_RATIO_THRESHOLD}"
    return 0
  fi

  while IFS='|' read -r key total prompt_t completion_t source; do
    [[ -z "${key}" ]] && continue

    local max_tokens
    max_tokens="$(lookup_max_tokens "${key}")"
    max_tokens="${max_tokens:-0}"

    local effective_budget
    if [[ "${max_tokens}" -gt 0 ]]; then
      effective_budget="${max_tokens}"
    else
      effective_budget="${ADVISORY_MAX_TOKENS}"
    fi

    # Skip if total <= threshold * effective_budget.
    local threshold_value
    threshold_value=$((effective_budget * OVERRUN_RATIO_THRESHOLD))
    if [[ "${total}" -lt "${threshold_value}" ]]; then
      continue
    fi

    local ratio
    ratio="$(awk -v t="${total}" -v b="${effective_budget}" 'BEGIN { printf "%.2f", t / b }')"

    write_postmortem \
      "${key}" "${total}" "${prompt_t}" "${completion_t}" \
      "${source}" "${max_tokens}" "${ratio}" "${effective_budget}" >/dev/null

    rows+="| ${key} | ${total} | ${effective_budget} | ${ratio}x | ${source} |"$'\n'
    count=$((count + 1))
  done <<<"${records}"

  write_index "${count}" "${rows}"

  printf 'YARLI_TOKEN_POSTMORTEM_OK count=%d threshold_ratio=%s.0\n' \
    "${count}" "${OVERRUN_RATIO_THRESHOLD}"
}

main
