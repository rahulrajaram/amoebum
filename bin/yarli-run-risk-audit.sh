#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
CONTINUATION_FILE="${REPO_ROOT}/.yarli/continuation.json"
STATE_DIR="${REPO_ROOT}/.yarli"
REPORT_DIR="${REPO_ROOT}/.agent"
JSON_OUTPUT="${STATE_DIR}/run-risk-audit.json"
MARKDOWN_OUTPUT="${REPORT_DIR}/yarli-run-risk-audit.md"
LINE_COUNT_AUDIT="${SCRIPT_DIR}/line-count-audit.sh"
CONFIG_FILE="${REPO_ROOT}/yarli.toml"
HARD_STOP_TOKEN_THRESHOLD=200000

usage() {
  cat <<'EOF'
Usage:
  bin/yarli-run-risk-audit.sh

Writes:
  ./.yarli/run-risk-audit.json
  ./.agent/yarli-run-risk-audit.md

The audit is advisory. It exits non-zero only on setup or parsing failures.
EOF
}

fail() {
  printf 'YARLI_RUN_RISK_AUDIT_ERROR: %s\n' "$*" >&2
  exit 1
}

require_cmd() {
  local cmd="$1"
  command -v "${cmd}" >/dev/null 2>&1 || fail "missing required command: ${cmd}"
}

numeric_or_default() {
  local raw="${1:-}"
  local fallback="$2"
  if [[ "${raw}" =~ ^-?[0-9]+$ ]]; then
    printf '%s\n' "${raw}"
  else
    printf '%s\n' "${fallback}"
  fi
}

toml_get_run_value() {
  local key="$1"
  awk -v key="${key}" '
function trim(s) {
  sub(/^[[:space:]]+/, "", s)
  sub(/[[:space:]]+$/, "", s)
  return s
}
BEGIN {
  in_section = 0
}
{
  line = $0
  sub(/[[:space:]]*#.*/, "", line)
  if (line ~ /^[[:space:]]*\[[^]]+\][[:space:]]*$/) {
    section_name = line
    gsub(/^[[:space:]]*\[/, "", section_name)
    gsub(/\][[:space:]]*$/, "", section_name)
    in_section = (section_name == "run")
    next
  }
  if (!in_section) {
    next
  }
  if (line ~ "^[[:space:]]*" key "[[:space:]]*=") {
    sub("^[[:space:]]*" key "[[:space:]]*=[[:space:]]*", "", line)
    print trim(line)
    exit
  }
}
' "${CONFIG_FILE}"
}

classify_token_pressure() {
  local tranche_count="$1"
  local target_breaches="$2"
  local max_breaches="$3"
  local max_total_tokens="$4"
  awk \
    -v tranche_count="${tranche_count}" \
    -v target_breaches="${target_breaches}" \
    -v max_breaches="${max_breaches}" \
    -v max_total_tokens="${max_total_tokens}" '
BEGIN {
  rating = "low"
  if (max_breaches >= 1 || max_total_tokens >= 150000) {
    rating = "high"
  } else if (target_breaches >= 1 || tranche_count >= 2) {
    rating = "medium"
  }
  print rating
}'
}

classify_file_pressure() {
  local fail_count="$1"
  local near_limit_count="$2"
  awk -v fail_count="${fail_count}" -v near_limit_count="${near_limit_count}" '
BEGIN {
  rating = "low"
  if (fail_count >= 2) {
    rating = "high"
  } else if (fail_count >= 1 || near_limit_count >= 2) {
    rating = "medium"
  }
  print rating
}'
}

build_risk_decision() {
  local token_pressure="$1"
  local file_pressure="$2"
  local oom_present="$3"
  local failed_or_cancelled="$4"
  local max_auto_advance="$5"
  awk \
    -v token_pressure="${token_pressure}" \
    -v file_pressure="${file_pressure}" \
    -v oom_present="${oom_present}" \
    -v failed_or_cancelled="${failed_or_cancelled}" \
    -v max_auto_advance="${max_auto_advance}" '
function weight(label) {
  if (label == "high") return 2
  if (label == "medium") return 1
  return 0
}
BEGIN {
  score = weight(token_pressure) + weight(file_pressure)
  if (oom_present == "true") score += 2
  if (failed_or_cancelled == "true") score += 1

  continuation = "safe-single-tranche"
  burst = "avoid"
  overall = "low"
  rationale = "token history, file pressure, and recent exits are calm enough for a bounded continuation"

  if (score >= 4) {
    continuation = "review-before-continue"
    burst = "block"
    overall = "high"
    rationale = "recent local evidence shows token-heavy or memory-pressure-prone churn that should not auto-advance"
  } else if (score >= 2) {
    continuation = "single-tranche-only"
    burst = "block"
    overall = "medium"
    rationale = "continue only as a bounded single tranche until token and file pressure cool off"
  }

  if (max_auto_advance == 0 && burst != "allow") {
    rationale = rationale " (config currently permits unlimited auto-advance, which is too loose for this risk profile)"
  } else if (max_auto_advance > 1 && burst != "allow") {
    rationale = rationale " (config should stay at one auto-advanced tranche or lower)"
  }

  printf "%s|%s|%s|%s\n", overall, continuation, burst, rationale
}'
}

extract_documented_token_run_records() {
  awk '
function emit(key, tokens, raw) {
  if (key != "" && tokens ~ /^[0-9]+$/) {
    printf "%s|%s|%s:%s:%s\n", key, tokens, FILENAME, FNR, raw
  }
}
{
  raw = $0
  rest = $0
  while (match(rest, /NXT-[0-9]+/)) {
    key_start = RSTART
    key_length = RLENGTH
    key = substr(rest, key_start, key_length)
    after = substr(rest, key_start + key_length)
    if (match(after, /NXT-[0-9]+/)) {
      after = substr(after, 1, RSTART - 1)
    }
    token = ""
    if (match(after, /(consumed|used)[^0-9`]*`?[0-9][0-9][0-9][0-9][0-9][0-9]*`?[[:space:]]+tokens/)) {
      segment = substr(after, RSTART, RLENGTH)
      if (match(segment, /[0-9][0-9][0-9][0-9][0-9][0-9]*/)) {
        token = substr(segment, RSTART, RLENGTH)
      }
    }
    emit(key, token, raw)
    rest = substr(rest, key_start + key_length)
  }
}
' "$@"
}

dedupe_token_run_records() {
  awk -F'|' '
NF >= 3 && $1 != "" {
  if (!seen_order[$1]++) {
    order[++count] = $1
  }
  records[$1] = $0
}
END {
  for (i = 1; i <= count; i++) {
    print records[order[i]]
  }
}
'
}

evaluate_hard_stop_token_runs() {
  local threshold="$1"
  awk -F'|' -v threshold="${threshold}" '
NF >= 3 && $1 != "" {
  count += 1
  keys[count] = $1
  totals[count] = $2 + 0
  records[count] = $0
}
END {
  triggered = "false"
  if (count >= 2 && totals[count - 1] >= threshold && totals[count] >= threshold) {
    triggered = "true"
  }
  printf "%s|%d|%s|%d|%s|%d\n",
    triggered,
    count,
    (count >= 2 ? keys[count - 1] : ""),
    (count >= 2 ? totals[count - 1] : 0),
    (count >= 1 ? keys[count] : ""),
    (count >= 1 ? totals[count] : 0)
}
'
}

[[ "${1:-}" =~ ^(--help|-h)$ ]] && {
  usage
  exit 0
}

require_cmd jq
require_cmd awk
require_cmd wc
require_cmd rg
[[ -x "${LINE_COUNT_AUDIT}" ]] || fail "missing executable audit helper: ${LINE_COUNT_AUDIT}"
[[ -f "${CONFIG_FILE}" ]] || fail "missing config file: ${CONFIG_FILE}"
[[ -f "${CONTINUATION_FILE}" ]] || fail "missing continuation file: ${CONTINUATION_FILE}"

mkdir -p "${STATE_DIR}" "${REPORT_DIR}"

generated_at="$(date -u +"%Y-%m-%dT%H:%M:%SZ")"
generated_day="$(date -u +"%Y-%m-%d")"
run_id="$(jq -r '.run_id // ""' "${CONTINUATION_FILE}")"
exit_state="$(jq -r '.exit_state // "unknown"' "${CONTINUATION_FILE}")"
exit_reason="$(jq -r '.exit_reason // "unknown"' "${CONTINUATION_FILE}")"
max_auto_advance_raw="$(toml_get_run_value max_auto_advance_tranches || true)"
max_auto_advance="${max_auto_advance_raw//\"/}"
max_auto_advance="$(numeric_or_default "${max_auto_advance}" 1)"

target_threshold="$(numeric_or_default "$(jq -r '.tranche_token_thresholds.target_tokens // 70000' "${CONTINUATION_FILE}")" 70000)"
max_threshold="$(numeric_or_default "$(jq -r '.tranche_token_thresholds.max_recommended_tokens // 100000' "${CONTINUATION_FILE}")" 100000)"
tranche_count="$(numeric_or_default "$(jq -r '(.tranche_token_usage // []) | length' "${CONTINUATION_FILE}")" 0)"
max_total_tokens="$(numeric_or_default "$(jq -r '(.tranche_token_usage // []) | map(.total_tokens // 0) | max // 0' "${CONTINUATION_FILE}")" 0)"
target_breaches="$(jq -r --argjson target "${target_threshold}" '(.tranche_token_usage // []) | map(select((.total_tokens // 0) >= $target)) | length' "${CONTINUATION_FILE}")"
max_breaches="$(jq -r --argjson target "${max_threshold}" '(.tranche_token_usage // []) | map(select((.total_tokens // 0) >= $target)) | length' "${CONTINUATION_FILE}")"
latest_tranche_key="$(jq -r '(.tranche_token_usage // []) | last | .tranche_key // "unknown"' "${CONTINUATION_FILE}")"
latest_total_tokens="$(numeric_or_default "$(jq -r '(.tranche_token_usage // []) | last | .total_tokens // 0' "${CONTINUATION_FILE}")" 0)"
latest_warning="$(jq -r '(.tranche_token_usage // []) | last | .warning // ""' "${CONTINUATION_FILE}")"
target_breaches="$(numeric_or_default "${target_breaches}" 0)"
max_breaches="$(numeric_or_default "${max_breaches}" 0)"
current_token_run_records="$(jq -r '(.tranche_token_usage // [])[]? | select(.tranche_key != null) | "\(.tranche_key)|\(.total_tokens // 0)|.yarli/continuation.json:tranche_token_usage:\(.tranche_key)"' "${CONTINUATION_FILE}")"
documented_token_run_records="$(extract_documented_token_run_records "${REPO_ROOT}/IMPLEMENTATION_PLAN.md" "${REPO_ROOT}/PROMPT.md" 2>/dev/null || true)"
recent_token_run_records="$(
  {
    printf '%s\n' "${documented_token_run_records}"
    printf '%s\n' "${current_token_run_records}"
  } | dedupe_token_run_records
)"
hard_stop_evidence="$(printf '%s\n' "${recent_token_run_records}" | evaluate_hard_stop_token_runs "${HARD_STOP_TOKEN_THRESHOLD}")"
hard_stop_triggered="${hard_stop_evidence%%|*}"
rest="${hard_stop_evidence#*|}"
recent_token_run_count="${rest%%|*}"
rest="${rest#*|}"
hard_stop_previous_tranche_key="${rest%%|*}"
rest="${rest#*|}"
hard_stop_previous_total_tokens="${rest%%|*}"
rest="${rest#*|}"
hard_stop_latest_tranche_key="${rest%%|*}"
rest="${rest#*|}"
hard_stop_latest_total_tokens="${rest}"
recent_token_run_count="$(numeric_or_default "${recent_token_run_count}" 0)"
hard_stop_previous_total_tokens="$(numeric_or_default "${hard_stop_previous_total_tokens}" 0)"
hard_stop_latest_total_tokens="$(numeric_or_default "${hard_stop_latest_total_tokens}" 0)"
hard_stop_previous_record="$(printf '%s\n' "${recent_token_run_records}" | awk -F'|' -v key="${hard_stop_previous_tranche_key}" '$1 == key { record = $0 } END { print record }')"
hard_stop_latest_record="$(printf '%s\n' "${recent_token_run_records}" | awk -F'|' -v key="${hard_stop_latest_tranche_key}" '$1 == key { record = $0 } END { print record }')"
documented_overrun_lines="$(rg -n 'NXT-[0-9]+.*154833|154833.*NXT-[0-9]+' "${REPO_ROOT}/PROMPT.md" "${REPO_ROOT}/IMPLEMENTATION_PLAN.md" 2>/dev/null || true)"
documented_overrun_count="$(numeric_or_default "$(printf '%s\n' "${documented_overrun_lines}" | awk 'NF { count += 1 } END { print count + 0 }')" 0)"
documented_overrun_max_tokens=0
documented_target_breaches=0
documented_max_breaches=0
if [[ "${documented_overrun_count}" -gt 0 ]]; then
  # NXT-476 exists specifically because the NXT-422 run consumed 154833 tokens.
  documented_overrun_max_tokens=154833
  [[ "${documented_overrun_max_tokens}" -ge "${target_threshold}" ]] && documented_target_breaches=1
  [[ "${documented_overrun_max_tokens}" -ge "${max_threshold}" ]] && documented_max_breaches=1
fi
effective_target_breaches=$((target_breaches + documented_target_breaches))
effective_max_breaches=$((max_breaches + documented_max_breaches))
effective_max_total_tokens="${max_total_tokens}"
if [[ "${documented_overrun_max_tokens}" -gt "${effective_max_total_tokens}" ]]; then
  effective_max_total_tokens="${documented_overrun_max_tokens}"
fi
token_pressure="$(classify_token_pressure "${tranche_count}" "${effective_target_breaches}" "${effective_max_breaches}" "${effective_max_total_tokens}")"

line_count_output="$("${LINE_COUNT_AUDIT}" all 2>&1 || true)"
line_count_fail_count="$(numeric_or_default "$(printf '%s\n' "${line_count_output}" | rg -c 'status=FAIL' || true)" 0)"
line_count_near_limit_count="$(printf '%s\n' "${line_count_output}" | awk '
/^LINE_COUNT_AUDIT / {
  lines = ""
  max = ""
  for (i = 1; i <= NF; i++) {
    if ($i ~ /^lines=/) {
      split($i, a, "=")
      lines = a[2]
    } else if ($i ~ /^max=/) {
      split($i, a, "=")
      max = a[2]
    }
  }
  if (lines != "" && max != "" && max > 0) {
    ratio = lines / max
    if (ratio >= 0.95) count += 1
  }
}
END {
  print count + 0
}')"
line_count_near_limit_count="$(numeric_or_default "${line_count_near_limit_count}" 0)"
file_pressure="$(classify_file_pressure "${line_count_fail_count}" "${line_count_near_limit_count}")"
line_count_failures="$(printf '%s\n' "${line_count_output}" | rg '^LINE_COUNT_AUDIT .*status=FAIL' || true)"

symptom_text="$(jq -r '
  [
    .exit_state,
    .exit_reason,
    .cancellation_source,
    (.cancellation_provenance.signal_name // ""),
    (.cancellation_provenance.actor_detail // ""),
    (.cancellation_provenance.stage // ""),
    (.summary.cancelled // 0 | tostring),
    (.tasks[]?.last_error // ""),
    (.tasks[]?.blocker // "")
  ] | join("\n")
' "${CONTINUATION_FILE}")"

oom_present="false"
oom_signals=""
if printf '%s\n' "${symptom_text}" | rg -qi 'oom|out of memory|memory pressure|sigkill|killed|command killed'; then
  oom_present="true"
  oom_signals="$(printf '%s\n' "${symptom_text}" | rg -io 'oom|out of memory|memory pressure|sigkill|killed|command killed' | awk '!seen[$0]++')"
fi

failed_or_cancelled="false"
if [[ "${exit_state}" == "RunFailed" || "${exit_state}" == "RunCancelled" ]]; then
  failed_or_cancelled="true"
fi

decision="$(build_risk_decision "${token_pressure}" "${file_pressure}" "${oom_present}" "${failed_or_cancelled}" "${max_auto_advance}")"
overall_risk="${decision%%|*}"
rest="${decision#*|}"
continuation_class="${rest%%|*}"
rest="${rest#*|}"
burst_guidance="${rest%%|*}"
decision_rationale="${rest#*|}"

recommendation="Continue with the next tranche only."
if [[ "${continuation_class}" == "review-before-continue" ]]; then
  recommendation="Pause auto-continuation, shrink scope or ratchet file pressure first, and resume only after reviewing the evidence."
elif [[ "${continuation_class}" == "single-tranche-only" ]]; then
  recommendation="Continue with one tranche at a time and avoid grouped or bursty follow-on runs until the pressure drops."
fi

fresh_launch_guidance="allow-fresh-launch"
if [[ "${overall_risk}" == "high" || "${burst_guidance}" == "block" ]]; then
  fresh_launch_guidance="manual-review-before-fresh-launch"
fi

if [[ "${hard_stop_triggered}" == "true" ]]; then
  overall_risk="high"
  continuation_class="hard-stop"
  burst_guidance="block"
  fresh_launch_guidance="do-not-fresh-launch"
  decision_rationale="two consecutive recent tranche runs exceeded ${HARD_STOP_TOKEN_THRESHOLD} tokens (${hard_stop_previous_tranche_key}=${hard_stop_previous_total_tokens}, ${hard_stop_latest_tranche_key}=${hard_stop_latest_total_tokens}); do not fresh-launch another run until scope is reduced and the token pattern is reviewed"
  recommendation="Hard stop: do not fresh-launch another Yarli run until the consecutive >${HARD_STOP_TOKEN_THRESHOLD}-token tranche pattern is reviewed and the next scope is reduced."
fi

json_payload="$(
  jq -n \
    --arg generated_at "${generated_at}" \
    --arg generated_day "${generated_day}" \
    --arg run_id "${run_id}" \
    --arg exit_state "${exit_state}" \
    --arg exit_reason "${exit_reason}" \
    --arg overall_risk "${overall_risk}" \
    --arg continuation_class "${continuation_class}" \
    --arg burst_guidance "${burst_guidance}" \
    --arg fresh_launch_guidance "${fresh_launch_guidance}" \
    --arg decision_rationale "${decision_rationale}" \
    --arg recommendation "${recommendation}" \
    --arg token_pressure "${token_pressure}" \
    --arg latest_tranche_key "${latest_tranche_key}" \
    --arg latest_warning "${latest_warning}" \
    --arg documented_overrun_lines "${documented_overrun_lines}" \
    --arg recent_token_run_records "${recent_token_run_records}" \
    --arg hard_stop_triggered "${hard_stop_triggered}" \
    --arg hard_stop_previous_tranche_key "${hard_stop_previous_tranche_key}" \
    --arg hard_stop_latest_tranche_key "${hard_stop_latest_tranche_key}" \
    --arg hard_stop_previous_record "${hard_stop_previous_record}" \
    --arg hard_stop_latest_record "${hard_stop_latest_record}" \
    --arg file_pressure "${file_pressure}" \
    --arg line_count_output "${line_count_output}" \
    --arg line_count_failures "${line_count_failures}" \
    --arg oom_present "${oom_present}" \
    --arg oom_signals "${oom_signals}" \
    --argjson max_auto_advance "${max_auto_advance}" \
    --argjson tranche_count "${tranche_count}" \
    --argjson latest_total_tokens "${latest_total_tokens}" \
    --argjson max_total_tokens "${max_total_tokens}" \
    --argjson effective_max_total_tokens "${effective_max_total_tokens}" \
    --argjson target_threshold "${target_threshold}" \
    --argjson max_threshold "${max_threshold}" \
    --argjson target_breaches "${target_breaches}" \
    --argjson max_breaches "${max_breaches}" \
    --argjson documented_overrun_count "${documented_overrun_count}" \
    --argjson documented_overrun_max_tokens "${documented_overrun_max_tokens}" \
    --argjson documented_target_breaches "${documented_target_breaches}" \
    --argjson documented_max_breaches "${documented_max_breaches}" \
    --argjson effective_target_breaches "${effective_target_breaches}" \
    --argjson effective_max_breaches "${effective_max_breaches}" \
    --argjson hard_stop_token_threshold "${HARD_STOP_TOKEN_THRESHOLD}" \
    --argjson recent_token_run_count "${recent_token_run_count}" \
    --argjson hard_stop_previous_total_tokens "${hard_stop_previous_total_tokens}" \
    --argjson hard_stop_latest_total_tokens "${hard_stop_latest_total_tokens}" \
    --argjson line_count_fail_count "${line_count_fail_count}" \
    --argjson line_count_near_limit_count "${line_count_near_limit_count}" \
    '
{
  schema_version: 1,
  generated_at: $generated_at,
  run_id: $run_id,
  overall_risk: $overall_risk,
  continuation_class: $continuation_class,
  burst_guidance: $burst_guidance,
  fresh_launch_guidance: $fresh_launch_guidance,
  decision_rationale: $decision_rationale,
  recommendation: $recommendation,
  config: {
    max_auto_advance_tranches: $max_auto_advance
  },
  token_history: {
    pressure: $token_pressure,
    tranche_count: $tranche_count,
    latest_tranche_key: $latest_tranche_key,
    latest_total_tokens: $latest_total_tokens,
    max_total_tokens: $max_total_tokens,
    effective_max_total_tokens: $effective_max_total_tokens,
    target_tokens: $target_threshold,
    max_recommended_tokens: $max_threshold,
    target_breaches: $target_breaches,
    max_breaches: $max_breaches,
    effective_target_breaches: $effective_target_breaches,
    effective_max_breaches: $effective_max_breaches,
    latest_warning: $latest_warning,
    hard_stop: {
      triggered: ($hard_stop_triggered == "true"),
      threshold_tokens: $hard_stop_token_threshold,
      recent_token_run_count: $recent_token_run_count,
      previous_tranche_key: $hard_stop_previous_tranche_key,
      previous_total_tokens: $hard_stop_previous_total_tokens,
      latest_tranche_key: $hard_stop_latest_tranche_key,
      latest_total_tokens: $hard_stop_latest_total_tokens,
      consecutive_records: ([$hard_stop_previous_record, $hard_stop_latest_record] | map(select(length > 0))),
      recent_records: ($recent_token_run_records | split("\n") | map(select(length > 0)))
    },
    documented_overruns: {
      count: $documented_overrun_count,
      max_total_tokens: $documented_overrun_max_tokens,
      target_breaches: $documented_target_breaches,
      max_breaches: $documented_max_breaches,
      records: ($documented_overrun_lines | split("\n") | map(select(length > 0)))
    }
  },
  file_pressure: {
    pressure: $file_pressure,
    fail_count: $line_count_fail_count,
    near_limit_count: $line_count_near_limit_count,
    failing_rules: ($line_count_failures | split("\n") | map(select(length > 0)))
  },
  recent_exit: {
    exit_state: $exit_state,
    exit_reason: $exit_reason,
    memory_pressure_symptoms_present: ($oom_present == "true"),
    memory_pressure_signals: ($oom_signals | split("\n") | map(select(length > 0)))
  },
  raw_line_count_audit: ($line_count_output | split("\n") | map(select(length > 0)))
}'
)"

printf '%s\n' "${json_payload}" > "${JSON_OUTPUT}"

{
  printf '# Yarli Run-Risk Audit\n\n'
  printf '%s\n' "- Generated: \`${generated_at}\`"
  printf '%s\n' "- Run: \`${run_id:-unknown}\`"
  printf '%s\n' "- Overall risk: \`${overall_risk}\`"
  printf '%s\n' "- Continuation class: \`${continuation_class}\`"
  printf '%s\n' "- Multi-tranche burst guidance: \`${burst_guidance}\`"
  printf '%s\n' "- Fresh-launch guidance: \`${fresh_launch_guidance}\`"
  printf '%s\n' "- Recommendation: ${recommendation}"
  printf '\n## Evidence\n'
  printf '%s\n' "- Token history: \`${token_pressure}\` pressure; latest tranche \`${latest_tranche_key}\` used \`${latest_total_tokens}\` tokens; current-continuation max \`${max_total_tokens}\`; effective max including documented overrun context \`${effective_max_total_tokens}\`; breaches target/max = \`${effective_target_breaches}/${effective_max_breaches}\`."
  if [[ "${hard_stop_triggered}" == "true" ]]; then
    printf '%s\n' "- Hard-stop token gate: last two known tranche-run token records exceed \`${HARD_STOP_TOKEN_THRESHOLD}\` tokens: \`${hard_stop_previous_tranche_key}\` used \`${hard_stop_previous_total_tokens}\`, then \`${hard_stop_latest_tranche_key}\` used \`${hard_stop_latest_total_tokens}\`."
  fi
  if [[ "${documented_overrun_count}" -gt 0 ]]; then
    printf '%s\n' "- Documented overrun context: \`NXT-422\` consumed \`${documented_overrun_max_tokens}\` tokens against max recommended \`${max_threshold}\`; keep the next runtime wave split/fallback scoped instead of normalizing another over-budget run."
  fi
  printf '%s\n' "- File pressure: \`${file_pressure}\`; failing ceilings \`${line_count_fail_count}\`; near-limit files \`${line_count_near_limit_count}\`."
  printf '%s\n' "- Recent exit: \`${exit_state}\` / \`${exit_reason}\`; memory-pressure symptoms present = \`${oom_present}\`."
  if [[ -n "${oom_signals}" ]]; then
    printf '%s\n' "- Memory-pressure signals: \`$(printf '%s' "${oom_signals}" | paste -sd ', ' -)\`."
  fi
  printf '%s\n' "- Auto-advance cap from \`yarli.toml\`: \`${max_auto_advance}\`."
  printf '\n## Rationale\n'
  printf '%s\n' "- ${decision_rationale}."
  if [[ -n "${latest_warning}" ]]; then
    printf '%s\n' "- Latest tranche warning: \`${latest_warning}\`."
  fi
  if [[ "${hard_stop_triggered}" == "true" ]]; then
    printf '\n## Hard-Stop Records\n'
    printf '%s\n' "- \`${hard_stop_previous_record}\`"
    printf '%s\n' "- \`${hard_stop_latest_record}\`"
  fi
  if [[ -n "${documented_overrun_lines}" ]]; then
    printf '\n## Documented Overrun Records\n'
    while IFS= read -r line; do
      [[ -n "${line}" ]] || continue
      printf '%s\n' "- \`${line}\`"
    done <<< "${documented_overrun_lines}"
  fi
  if [[ -n "${line_count_failures}" ]]; then
    printf '\n## Failing File Ceilings\n'
    while IFS= read -r line; do
      [[ -n "${line}" ]] || continue
      printf '%s\n' "- \`${line}\`"
    done <<< "${line_count_failures}"
  fi
  printf '\n## Artifacts\n'
  printf '%s\n' "- JSON summary: \`${JSON_OUTPUT}\`"
  printf '%s\n' "- Continuation source: \`${CONTINUATION_FILE}\`"
  postmortem_index="${REPO_ROOT}/.agent/token-postmortems/INDEX.md"
  if [[ -f "${postmortem_index}" ]]; then
    postmortem_count="$(grep -cE '^\| NXT-' "${postmortem_index}" 2>/dev/null || printf '0')"
    printf '%s\n' "- Token-budget postmortems (${postmortem_count}): \`${postmortem_index}\`"
  else
    printf '%s\n' "- Token-budget postmortems: not yet generated; run \`bin/yarli-token-postmortem.sh\` to refresh"
  fi
} > "${MARKDOWN_OUTPUT}"

printf 'YARLI_RUN_RISK_AUDIT_OK overall=%s continuation=%s burst=%s fresh_launch=%s token_pressure=%s file_pressure=%s oom_symptoms=%s report=%s json=%s\n' \
  "${overall_risk}" \
  "${continuation_class}" \
  "${burst_guidance}" \
  "${fresh_launch_guidance}" \
  "${token_pressure}" \
  "${file_pressure}" \
  "${oom_present}" \
  "${MARKDOWN_OUTPUT}" \
  "${JSON_OUTPUT}"
