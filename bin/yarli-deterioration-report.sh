#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

CONFIG_FILE="${REPO_ROOT}/yarli.toml"
OUTPUT_FILE="${REPO_ROOT}/.agent/deterioration-report.md"
WINDOW_RUNS=20
DRY_RUN=0
SYNTHETIC_PROFILE=""
ASSERT_ACTION=""
AUDIT_FILE_INPUT=""

usage() {
  printf '%s\n' "Usage:"
  printf '%s\n' "  bin/yarli-deterioration-report.sh [--window-runs <n>] [--output <path>] [--dry-run]"
  printf '%s\n' "  bin/yarli-deterioration-report.sh --synthetic-profile <observe|retry|remediate|escalate> [--assert-action <class>]"
  printf '%s\n' ""
  printf '%s\n' "Options:"
  printf '%s\n' "  --config <path>            Path to yarli.toml (default: ./yarli.toml)"
  printf '%s\n' "  --window-runs <n>          Number of recent runs to analyze (default: 20)"
  printf '%s\n' "  --output <path>            Report file path (default: ./.agent/deterioration-report.md)"
  printf '%s\n' "  --audit-file <path>        Override audit file path (default: from yarli.toml)"
  printf '%s\n' "  --dry-run                  Print report to stdout without writing output file"
  printf '%s\n' "  --synthetic-profile <id>   Run synthetic classification probe only"
  printf '%s\n' "  --assert-action <class>    Expected class for synthetic or real classification"
  printf '%s\n' "  -h, --help                 Show this help"
}

fail() {
  printf 'YARLI_DETERIORATION_REPORT_ERROR: %s\n' "$*" >&2
  exit 1
}

require_file() {
  local path="$1"
  [[ -f "${path}" ]] || fail "missing file: ${path}"
}

strip_quotes() {
  local value="$1"
  value="${value%\"}"
  value="${value#\"}"
  printf '%s' "${value}"
}

normalize_signature() {
  local raw="$1"
  raw="$(printf '%s' "${raw}" | tr '\n' ' ' | sed -E 's/[[:space:]]+/ /g; s/^ //; s/ $//')"
  printf '%s' "${raw}" \
    | tr '[:upper:]' '[:lower:]' \
    | sed -E 's/[0-9a-f]{8}-[0-9a-f-]{27}/<id>/g; s/[^a-z0-9]+/-/g; s/^-+//; s/-+$//'
}

toml_get_value() {
  local file="$1"
  local section="$2"
  local key="$3"
  awk -v section="${section}" -v key="${key}" '
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
    in_section = (section_name == section)
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
' "${file}"
}

extract_run_id_from_status() {
  awk '/^Run / { print $2; exit }'
}

extract_state_from_status() {
  awk '/^State:/ { print $2; exit }'
}

extract_updated_from_status() {
  sed -nE 's/^Updated:[[:space:]]+//p' | head -n1
}

extract_tasks_summary() {
  sed -nE 's/^Tasks:[[:space:]]+([0-9]+) total \(([0-9]+) complete, ([0-9]+) failed, ([0-9]+) blocked, ([0-9]+) active\).*/\1 \2 \3 \4 \5/p' | head -n1
}

extract_deterioration_summary() {
  sed -nE 's/^Deterioration:[[:space:]]+score=([0-9]+(\.[0-9]+)?) trend=([^[:space:]]+) window_size=([0-9]+).*/\1 \3 \4/p' | head -n1
}

extract_failed_gates() {
  sed -nE 's/^Failed gates:[[:space:]]+([0-9]+).*/\1/p' | head -n1
}

extract_blocking_tasks_count() {
  sed -nE 's/^Blocking tasks:[[:space:]]+([0-9]+).*/\1/p' | head -n1
}

extract_blocker_lines() {
  awk '
BEGIN {
  in_blockers = 0
}
/^Blocking tasks:$/ {
  in_blockers = 1
  next
}
in_blockers && /^[[:space:]]*$/ {
  in_blockers = 0
  next
}
in_blockers && /^Suggested actions:$/ {
  in_blockers = 0
  next
}
in_blockers && /^Sequence deterioration:/ {
  in_blockers = 0
  next
}
in_blockers {
  line = $0
  sub(/^[[:space:]]+/, "", line)
  if (length(line) > 0) {
    print line
  }
}
'
}

float_add() {
  local a="$1"
  local b="$2"
  awk -v a="${a}" -v b="${b}" 'BEGIN { printf "%.6f", a + b }'
}

float_max() {
  local a="$1"
  local b="$2"
  awk -v a="${a}" -v b="${b}" 'BEGIN { if (b > a) printf "%.6f", b; else printf "%.6f", a }'
}

delta_float_or_na() {
  local current="$1"
  local previous="$2"
  if [[ -z "${previous}" ]]; then
    printf 'n/a'
    return 0
  fi
  awk -v current="${current}" -v previous="${previous}" 'BEGIN { printf "%.2f", current - previous }'
}

delta_int_or_na() {
  local current="$1"
  local previous="$2"
  if [[ -z "${previous}" ]]; then
    printf 'n/a'
    return 0
  fi
  awk -v current="${current}" -v previous="${previous}" 'BEGIN { printf "%d", current - previous }'
}

classify_action() {
  local avg_score="$1"
  local max_score="$2"
  local failed_runs="$3"
  local cancelled_runs="$4"
  local blocker_runs="$5"
  local failed_gates_total="$6"
  local retry_total="$7"
  local trend_rising="$8"
  awk \
    -v avg_score="${avg_score}" \
    -v max_score="${max_score}" \
    -v failed_runs="${failed_runs}" \
    -v cancelled_runs="${cancelled_runs}" \
    -v blocker_runs="${blocker_runs}" \
    -v failed_gates_total="${failed_gates_total}" \
    -v retry_total="${retry_total}" \
    -v trend_rising="${trend_rising}" '
BEGIN {
  severity = (avg_score * 1.2) + (max_score * 0.8) + (failed_runs * 2.0) + (cancelled_runs * 0.7) + (blocker_runs * 1.5) + (failed_gates_total * 2.5) + (retry_total * 0.4) + (trend_rising * 1.0)
  action = "observe"
  if (severity >= 14.0 || max_score >= 8.0 || failed_runs >= 3 || failed_gates_total >= 2) {
    action = "escalate"
  } else if (severity >= 8.0 || max_score >= 5.0 || (failed_runs >= 1 && retry_total >= 3) || blocker_runs >= 2 || trend_rising >= 2) {
    action = "remediate"
  } else if (severity >= 4.0 || failed_runs >= 1 || cancelled_runs >= 2 || retry_total >= 2) {
    action = "retry"
  }
  printf "%s|%.2f\n", action, severity
}
'
}

run_synthetic_probe() {
  local profile="$1"
  local assert_action="${2:-}"
  local avg_score=""
  local max_score=""
  local failed_runs=0
  local cancelled_runs=0
  local blocker_runs=0
  local failed_gates_total=0
  local retry_total=0
  local trend_rising=0

  case "${profile}" in
    observe)
      avg_score="0.40"
      max_score="1.00"
      ;;
    retry)
      avg_score="1.80"
      max_score="2.50"
      failed_runs=1
      cancelled_runs=1
      retry_total=2
      ;;
    remediate)
      avg_score="2.80"
      max_score="5.40"
      failed_runs=1
      cancelled_runs=0
      blocker_runs=1
      retry_total=3
      trend_rising=1
      ;;
    escalate)
      avg_score="6.50"
      max_score="9.10"
      failed_runs=3
      cancelled_runs=2
      blocker_runs=3
      failed_gates_total=2
      retry_total=6
      trend_rising=2
      ;;
    *)
      fail "unknown synthetic profile: ${profile}"
      ;;
  esac

  local classification
  local action
  local severity

  classification="$(classify_action "${avg_score}" "${max_score}" "${failed_runs}" "${cancelled_runs}" "${blocker_runs}" "${failed_gates_total}" "${retry_total}" "${trend_rising}")"
  action="${classification%%|*}"
  severity="${classification#*|}"

  if [[ -n "${assert_action}" && "${action}" != "${assert_action}" ]]; then
    fail "synthetic profile ${profile} classified as ${action}, expected ${assert_action}"
  fi

  printf 'YARLI_DETERIORATION_SYNTHETIC_OK: profile=%s action=%s severity=%s\n' \
    "${profile}" "${action}" "${severity}"
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --config)
      [[ $# -ge 2 ]] || fail "--config requires a value"
      CONFIG_FILE="$2"
      shift 2
      ;;
    --window-runs)
      [[ $# -ge 2 ]] || fail "--window-runs requires a value"
      WINDOW_RUNS="$2"
      shift 2
      ;;
    --output)
      [[ $# -ge 2 ]] || fail "--output requires a value"
      OUTPUT_FILE="$2"
      shift 2
      ;;
    --audit-file)
      [[ $# -ge 2 ]] || fail "--audit-file requires a value"
      AUDIT_FILE_INPUT="$2"
      shift 2
      ;;
    --dry-run)
      DRY_RUN=1
      shift
      ;;
    --synthetic-profile)
      [[ $# -ge 2 ]] || fail "--synthetic-profile requires a value"
      SYNTHETIC_PROFILE="$2"
      shift 2
      ;;
    --assert-action)
      [[ $# -ge 2 ]] || fail "--assert-action requires a value"
      ASSERT_ACTION="$2"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    --*)
      fail "unknown option: $1"
      ;;
    *)
      fail "unexpected positional argument: $1"
      ;;
  esac
done

if [[ -n "${ASSERT_ACTION}" ]]; then
  case "${ASSERT_ACTION}" in
    observe|retry|remediate|escalate)
      ;;
    *)
      fail "invalid --assert-action value: ${ASSERT_ACTION}"
      ;;
  esac
fi

if [[ -n "${SYNTHETIC_PROFILE}" ]]; then
  run_synthetic_probe "${SYNTHETIC_PROFILE}" "${ASSERT_ACTION}"
  exit 0
fi

if ! [[ "${WINDOW_RUNS}" =~ ^[0-9]+$ ]] || [[ "${WINDOW_RUNS}" -le 0 ]]; then
  fail "--window-runs must be a positive integer"
fi

require_file "${CONFIG_FILE}"

audit_file=""
if [[ -n "${AUDIT_FILE_INPUT}" ]]; then
  audit_file="${AUDIT_FILE_INPUT}"
else
  audit_file_raw="$(toml_get_value "${CONFIG_FILE}" "observability" "audit_file")"
  audit_file="$(strip_quotes "${audit_file_raw:-.yarl/audit.jsonl}")"
fi

if [[ "${audit_file}" != /* ]]; then
  audit_file="${REPO_ROOT}/${audit_file}"
fi

mapfile -t run_short_ids < <(
  yarli run list \
    | awk -v limit="${WINDOW_RUNS}" '
NR > 2 && $1 ~ /^[0-9a-f]+$/ {
  count += 1
  ids[count] = $1
}
END {
  start = count - limit + 1
  if (start < 1) {
    start = 1
  }
  for (i = count; i >= start; i -= 1) {
    print ids[i]
  }
}
'
)

if [[ ${#run_short_ids[@]} -eq 0 ]]; then
  fail "no runs found in yarli run list"
fi

declare -A blocker_signature_counts=()
declare -A failed_reason_counts=()
declare -A run_retry_counts=()
declare -a run_rows=()
declare -a run_ids=()

run_count=0
completed_runs=0
failed_runs=0
cancelled_runs=0
active_runs=0
unknown_state_runs=0
failed_task_runs=0
failed_gates_total=0
blocker_runs=0
trend_rising_count=0
trend_stable_count=0
trend_falling_count=0
trend_unknown_count=0
score_sum="0.000000"
max_score="0.000000"

for short_id in "${run_short_ids[@]}"; do
  run_status="$(yarli run status "${short_id}" 2>/dev/null || true)"
  [[ -n "${run_status}" ]] || continue

  run_id="$(printf '%s\n' "${run_status}" | extract_run_id_from_status)"
  run_state="$(printf '%s\n' "${run_status}" | extract_state_from_status)"
  run_updated="$(printf '%s\n' "${run_status}" | extract_updated_from_status)"
  tasks_summary="$(printf '%s\n' "${run_status}" | extract_tasks_summary)"
  deterioration_summary="$(printf '%s\n' "${run_status}" | extract_deterioration_summary)"

  [[ -n "${run_id}" ]] || continue
  [[ -n "${run_state}" ]] || run_state="Unknown"
  [[ -n "${run_updated}" ]] || run_updated="unknown"

  tasks_total=0
  tasks_complete=0
  tasks_failed=0
  tasks_blocked=0
  tasks_active=0

  if [[ -n "${tasks_summary}" ]]; then
    read -r tasks_total tasks_complete tasks_failed tasks_blocked tasks_active <<< "${tasks_summary}"
  fi

  det_score="0.00"
  det_trend="Unknown"
  det_window=0
  if [[ -n "${deterioration_summary}" ]]; then
    read -r det_score det_trend det_window <<< "${deterioration_summary}"
  fi

  run_explain="$(yarli run explain-exit "${run_id}" 2>/dev/null || true)"
  failed_gates=0
  blocking_tasks=0
  if [[ -n "${run_explain}" ]]; then
    failed_gates_raw="$(printf '%s\n' "${run_explain}" | extract_failed_gates)"
    blocking_tasks_raw="$(printf '%s\n' "${run_explain}" | extract_blocking_tasks_count)"
    failed_gates="${failed_gates_raw:-0}"
    blocking_tasks="${blocking_tasks_raw:-0}"
  fi

  if [[ -n "${run_explain}" ]]; then
    mapfile -t blocker_lines < <(printf '%s\n' "${run_explain}" | extract_blocker_lines)
    for blocker_line in "${blocker_lines[@]}"; do
      signature="$(normalize_signature "${blocker_line}")"
      [[ -n "${signature}" ]] || continue
      blocker_signature_counts["${signature}"]=$(( ${blocker_signature_counts["${signature}"]:-0} + 1 ))
    done
  fi

  task_list="$(yarli task list "${run_id}" 2>/dev/null || true)"
  if [[ -n "${task_list}" ]]; then
    mapfile -t failed_reasons < <(
      printf '%s\n' "${task_list}" \
        | sed -nE 's/^  [0-9a-f-]+[[:space:]]+TaskFailed[[:space:]]+last=[^[:space:]]+[[:space:]]+reason=([^[:space:]]+).*/\1/p'
    )
    for reason in "${failed_reasons[@]}"; do
      failed_reason_counts["${reason}"]=$(( ${failed_reason_counts["${reason}"]:-0} + 1 ))
    done
  fi

  run_count=$((run_count + 1))
  run_ids+=("${run_id}")
  score_sum="$(float_add "${score_sum}" "${det_score}")"
  max_score="$(float_max "${max_score}" "${det_score}")"
  failed_gates_total=$((failed_gates_total + failed_gates))

  if [[ "${tasks_failed}" -gt 0 ]]; then
    failed_task_runs=$((failed_task_runs + 1))
  fi
  if [[ "${blocking_tasks}" -gt 0 ]]; then
    blocker_runs=$((blocker_runs + 1))
  fi

  case "${run_state}" in
    RunCompleted)
      completed_runs=$((completed_runs + 1))
      ;;
    RunFailed)
      failed_runs=$((failed_runs + 1))
      ;;
    RunCancelled)
      cancelled_runs=$((cancelled_runs + 1))
      ;;
    RunActive)
      active_runs=$((active_runs + 1))
      ;;
    *)
      unknown_state_runs=$((unknown_state_runs + 1))
      ;;
  esac

  case "${det_trend,,}" in
    rising)
      trend_rising_count=$((trend_rising_count + 1))
      ;;
    stable)
      trend_stable_count=$((trend_stable_count + 1))
      ;;
    falling|improving)
      trend_falling_count=$((trend_falling_count + 1))
      ;;
    *)
      trend_unknown_count=$((trend_unknown_count + 1))
      ;;
  esac

  run_rows+=("${run_id}|${run_state}|${run_updated}|${det_score}|${det_trend}|${tasks_failed}|${failed_gates}|${blocking_tasks}")
done

if [[ "${run_count}" -eq 0 ]]; then
  fail "no runs could be parsed from selected window"
fi

retry_events_total=0
if [[ -f "${audit_file}" ]]; then
  ids_file="$(mktemp)"
  printf '%s\n' "${run_ids[@]}" > "${ids_file}"
  audit_retry_summary="$(
    awk '
NR == FNR {
  ids[$1] = 1
  next
}
{
  run_id = ""
  attempt_no = 0
  if (match($0, /"run_id":"([^"]+)"/, m)) {
    run_id = m[1]
  }
  if (!(run_id in ids)) {
    next
  }
  if (match($0, /"attempt_no":([0-9]+)/, a)) {
    attempt_no = a[1] + 0
  }
  if (attempt_no >= 2) {
    retry_total += 1
    run_retry[run_id] += 1
  }
}
END {
  printf "retry_total=%d\n", retry_total + 0
  for (run_id in run_retry) {
    printf "run_retry=%s,%d\n", run_id, run_retry[run_id]
  }
}
' "${ids_file}" "${audit_file}"
  )"
  rm -f "${ids_file}"

  while IFS= read -r summary_line; do
    case "${summary_line}" in
      retry_total=*)
        retry_events_total="${summary_line#retry_total=}"
        ;;
      run_retry=*)
        pair="${summary_line#run_retry=}"
        run_id="${pair%,*}"
        retry_count="${pair#*,}"
        run_retry_counts["${run_id}"]="${retry_count}"
        ;;
    esac
  done <<< "${audit_retry_summary}"
fi

avg_score="$(awk -v sum="${score_sum}" -v n="${run_count}" 'BEGIN { if (n <= 0) printf "0.00"; else printf "%.2f", sum / n }')"
max_score_fmt="$(awk -v max_score="${max_score}" 'BEGIN { printf "%.2f", max_score }')"

classification="$(classify_action "${avg_score}" "${max_score_fmt}" "${failed_runs}" "${cancelled_runs}" "${blocker_runs}" "${failed_gates_total}" "${retry_events_total}" "${trend_rising_count}")"
action_class="${classification%%|*}"
severity_score="${classification#*|}"

if [[ -n "${ASSERT_ACTION}" && "${action_class}" != "${ASSERT_ACTION}" ]]; then
  fail "classification=${action_class} does not match --assert-action=${ASSERT_ACTION}"
fi

top_blockers_lines="$(
  {
    for key in "${!blocker_signature_counts[@]}"; do
      printf '%s\t%s\n' "${blocker_signature_counts["${key}"]}" "${key}"
    done
  } | sort -k1,1nr -k2,2 | head -n 3 | awk '{ printf "- %s (%s)\n", $2, $1 }'
)"
if [[ -z "${top_blockers_lines}" ]]; then
  top_blockers_lines="- none"
fi

top_failed_reasons_lines="$(
  {
    for key in "${!failed_reason_counts[@]}"; do
      printf '%s\t%s\n' "${failed_reason_counts["${key}"]}" "${key}"
    done
  } | sort -k1,1nr -k2,2 | head -n 3 | awk '{ printf "- %s (%s)\n", $2, $1 }'
)"
if [[ -z "${top_failed_reasons_lines}" ]]; then
  top_failed_reasons_lines="- none"
fi

top_retry_runs_lines="$(
  {
    for key in "${!run_retry_counts[@]}"; do
      printf '%s\t%s\n' "${run_retry_counts["${key}"]}" "${key}"
    done
  } | sort -k1,1nr -k2,2 | head -n 5 | awk '{ printf "- %s (%s)\n", $2, $1 }'
)"
if [[ -z "${top_retry_runs_lines}" ]]; then
  top_retry_runs_lines="- none"
fi

previous_snapshot_found=0
previous_snapshot_timestamp=""
previous_action_class=""
previous_severity_score=""
previous_avg_score=""
previous_max_score=""
previous_failed_task_runs=""
previous_retry_events_total=""
delta_severity_score="n/a"
delta_avg_score="n/a"
delta_max_score="n/a"
delta_failed_task_runs="n/a"
delta_retry_events_total="n/a"

if [[ -f "${OUTPUT_FILE}" ]]; then
  previous_snapshot="$(
    awk '
/^## YARLI_DETERIORATION_REPORT_V1 / {
  block = $0 ORS
  capture = 1
  next
}
capture {
  block = block $0 ORS
}
END {
  printf "%s", block
}
' "${OUTPUT_FILE}"
  )"
  if [[ -n "${previous_snapshot}" ]]; then
    previous_snapshot_found=1
    previous_snapshot_timestamp="$(printf '%s\n' "${previous_snapshot}" | sed -nE 's/^## YARLI_DETERIORATION_REPORT_V1[[:space:]]+(.+)$/\1/p' | head -n1)"
    previous_action_class="$(printf '%s\n' "${previous_snapshot}" | sed -nE 's/^- action_class:[[:space:]]+(.+)$/\1/p' | head -n1)"
    previous_severity_score="$(printf '%s\n' "${previous_snapshot}" | sed -nE 's/^- severity_score:[[:space:]]+([0-9]+(\.[0-9]+)?).*/\1/p' | head -n1)"
    previous_avg_score="$(printf '%s\n' "${previous_snapshot}" | sed -nE 's/^- deterioration_scores:[[:space:]]+avg=([0-9]+(\.[0-9]+)?)[[:space:]]+max=.*/\1/p' | head -n1)"
    previous_max_score="$(printf '%s\n' "${previous_snapshot}" | sed -nE 's/^- deterioration_scores:[[:space:]]+avg=[0-9]+(\.[0-9]+)?[[:space:]]+max=([0-9]+(\.[0-9]+)?).*/\2/p' | head -n1)"
    previous_failed_task_runs="$(printf '%s\n' "${previous_snapshot}" | sed -nE 's/^- failure_signals:[[:space:]]+failed_task_runs=([0-9]+).*/\1/p' | head -n1)"
    previous_retry_events_total="$(printf '%s\n' "${previous_snapshot}" | sed -nE 's/^- failure_signals:[[:space:]]+.*retry_events_total=([0-9]+).*/\1/p' | head -n1)"
    delta_severity_score="$(delta_float_or_na "${severity_score}" "${previous_severity_score}")"
    delta_avg_score="$(delta_float_or_na "${avg_score}" "${previous_avg_score}")"
    delta_max_score="$(delta_float_or_na "${max_score_fmt}" "${previous_max_score}")"
    delta_failed_task_runs="$(delta_int_or_na "${failed_task_runs}" "${previous_failed_task_runs}")"
    delta_retry_events_total="$(delta_int_or_na "${retry_events_total}" "${previous_retry_events_total}")"
  fi
fi

timestamp_utc="$(date -u +"%Y-%m-%dT%H:%M:%SZ")"
action_recommendation="continue normally; keep monitoring."
case "${action_class}" in
  observe)
    action_recommendation="continue normally; keep monitoring."
    ;;
  retry)
    action_recommendation="retry transient failures in-place before escalating."
    ;;
  remediate)
    action_recommendation="run focused remediation (for example bin/yarli-remediate-run.sh <run-id>) before advancing."
    ;;
  escalate)
    action_recommendation="pause automation and escalate to operator review immediately."
    ;;
esac

print_report() {
  printf '## YARLI_DETERIORATION_REPORT_V1 %s\n' "${timestamp_utc}"
  printf '\n'
  printf '### Window Summary\n'
  printf '\n'
  printf '%s\n' "- window_runs_requested: ${WINDOW_RUNS}"
  printf '%s\n' "- window_runs_analyzed: ${run_count}"
  printf '%s\n' "- source_run_data: yarli run status + yarli run explain-exit"
  printf '%s\n' "- source_task_data: yarli task list"
  printf '%s\n' "- source_audit_file: ${audit_file}"
  printf '%s\n' "- run_states: completed=${completed_runs} failed=${failed_runs} cancelled=${cancelled_runs} active=${active_runs} unknown=${unknown_state_runs}"
  printf '%s\n' "- deterioration_scores: avg=${avg_score} max=${max_score_fmt} trends(rising=${trend_rising_count} stable=${trend_stable_count} falling=${trend_falling_count} unknown=${trend_unknown_count})"
  printf '%s\n' "- failure_signals: failed_task_runs=${failed_task_runs} failed_gates_total=${failed_gates_total} blocker_runs=${blocker_runs} retry_events_total=${retry_events_total}"
  printf '\n'
  printf '### Classification\n'
  printf '\n'
  printf '%s\n' "- action_class: ${action_class}"
  printf '%s\n' "- severity_score: ${severity_score}"
  printf '%s\n' "- recommendation: ${action_recommendation}"
  printf '\n'
  printf '### Trend Comparison\n'
  printf '\n'
  if [[ "${previous_snapshot_found}" -eq 1 ]]; then
    printf '%s\n' "- previous_snapshot_timestamp: ${previous_snapshot_timestamp:-unknown}"
    printf '%s\n' "- previous_action_class: ${previous_action_class:-unknown}"
    printf '%s\n' "- previous_severity_score: ${previous_severity_score:-unknown}"
    printf '%s\n' "- delta_severity_score: ${delta_severity_score}"
    printf '%s\n' "- previous_avg_score: ${previous_avg_score:-unknown}"
    printf '%s\n' "- delta_avg_score: ${delta_avg_score}"
    printf '%s\n' "- previous_max_score: ${previous_max_score:-unknown}"
    printf '%s\n' "- delta_max_score: ${delta_max_score}"
    printf '%s\n' "- previous_failed_task_runs: ${previous_failed_task_runs:-unknown}"
    printf '%s\n' "- delta_failed_task_runs: ${delta_failed_task_runs}"
    printf '%s\n' "- previous_retry_events_total: ${previous_retry_events_total:-unknown}"
    printf '%s\n' "- delta_retry_events_total: ${delta_retry_events_total}"
  else
    printf '%s\n' '- previous_snapshot: none'
  fi
  printf '\n'
  printf '### Thresholds\n'
  printf '\n'
  printf '%s\n' '- observe: severity < 4 and no hard triggers.'
  printf '%s\n' '- retry: severity >= 4, or transient failure/retry pressure.'
  printf '%s\n' '- remediate: severity >= 8, or repeated blockers/retries, or max_score >= 5.'
  printf '%s\n' '- escalate: severity >= 14, or any hard trigger (max_score >= 8, failed_runs >= 3, failed_gates_total >= 2).'
  printf '\n'
  printf '### Top Signals\n'
  printf '\n'
  printf 'Task failure reasons:\n'
  printf '%s\n' "${top_failed_reasons_lines}"
  printf '\n'
  printf 'Blocker signatures:\n'
  printf '%s\n' "${top_blockers_lines}"
  printf '\n'
  printf 'Audit retry-heavy runs:\n'
  printf '%s\n' "${top_retry_runs_lines}"
  printf '\n'
  printf '### Run Trend Table\n'
  printf '\n'
  printf '| run_id | state | updated | score | trend | task_failed | failed_gates | blockers |\n'
  printf '| --- | --- | --- | ---: | --- | ---: | ---: | ---: |\n'
  for row in "${run_rows[@]}"; do
    IFS='|' read -r row_run_id row_state row_updated row_score row_trend row_task_failed row_failed_gates row_blockers <<< "${row}"
    printf '| %s | %s | %s | %s | %s | %s | %s | %s |\n' \
      "${row_run_id}" "${row_state}" "${row_updated}" "${row_score}" "${row_trend}" "${row_task_failed}" "${row_failed_gates}" "${row_blockers}"
  done
  printf '\n'
}

if [[ "${DRY_RUN}" -eq 1 ]]; then
  print_report
  printf 'YARLI_DETERIORATION_REPORT_OK: runs=%s action=%s severity=%s output=dry-run\n' \
    "${run_count}" "${action_class}" "${severity_score}"
  exit 0
fi

mkdir -p "$(dirname "${OUTPUT_FILE}")"
{
  print_report
} >> "${OUTPUT_FILE}"

printf 'YARLI_DETERIORATION_REPORT_OK: runs=%s action=%s severity=%s output=%s\n' \
  "${run_count}" "${action_class}" "${severity_score}" "${OUTPUT_FILE}"
