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

[[ "${1:-}" =~ ^(--help|-h)$ ]] && {
  usage
  exit 0
}

require_cmd jq
require_cmd awk
require_cmd wc
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
token_pressure="$(classify_token_pressure "${tranche_count}" "${target_breaches}" "${max_breaches}" "${max_total_tokens}")"

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
    --arg decision_rationale "${decision_rationale}" \
    --arg recommendation "${recommendation}" \
    --arg token_pressure "${token_pressure}" \
    --arg latest_tranche_key "${latest_tranche_key}" \
    --arg latest_warning "${latest_warning}" \
    --arg file_pressure "${file_pressure}" \
    --arg line_count_output "${line_count_output}" \
    --arg line_count_failures "${line_count_failures}" \
    --arg oom_present "${oom_present}" \
    --arg oom_signals "${oom_signals}" \
    --argjson max_auto_advance "${max_auto_advance}" \
    --argjson tranche_count "${tranche_count}" \
    --argjson latest_total_tokens "${latest_total_tokens}" \
    --argjson max_total_tokens "${max_total_tokens}" \
    --argjson target_threshold "${target_threshold}" \
    --argjson max_threshold "${max_threshold}" \
    --argjson target_breaches "${target_breaches}" \
    --argjson max_breaches "${max_breaches}" \
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
    target_tokens: $target_threshold,
    max_recommended_tokens: $max_threshold,
    target_breaches: $target_breaches,
    max_breaches: $max_breaches,
    latest_warning: $latest_warning
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
  printf '%s\n' "- Recommendation: ${recommendation}"
  printf '\n## Evidence\n'
  printf '%s\n' "- Token history: \`${token_pressure}\` pressure; latest tranche \`${latest_tranche_key}\` used \`${latest_total_tokens}\` tokens; max observed \`${max_total_tokens}\`; breaches target/max = \`${target_breaches}/${max_breaches}\`."
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
} > "${MARKDOWN_OUTPUT}"

printf 'YARLI_RUN_RISK_AUDIT_OK overall=%s continuation=%s burst=%s token_pressure=%s file_pressure=%s oom_symptoms=%s report=%s json=%s\n' \
  "${overall_risk}" \
  "${continuation_class}" \
  "${burst_guidance}" \
  "${token_pressure}" \
  "${file_pressure}" \
  "${oom_present}" \
  "${MARKDOWN_OUTPUT}" \
  "${JSON_OUTPUT}"
