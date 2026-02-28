#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

PLAN_FILE="${REPO_ROOT}/IMPLEMENTATION_PLAN.md"
CONTINUATION_FILE="${REPO_ROOT}/.yarli/continuation.json"
CONFIG_FILE="${REPO_ROOT}/yarli.toml"
PROMPT_FILE="${REPO_ROOT}/PROMPT.md"

usage() {
  cat <<'EOF'
Usage:
  bin/yarli-sanitize-continuation.sh

Purpose:
  Clears stale continuation state when IMPLEMENTATION_PLAN.md has zero open
  tranches, and optionally reconciles related run rows/events in Postgres.

Notes:
  - Reads settings from ./yarli.toml and objective from ./PROMPT.md.
  - Requires no arguments.
EOF
}

if [[ "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
  usage
  exit 0
fi

if [[ $# -ne 0 ]]; then
  echo "YARLI_CONTINUATION_SANITIZE_ERROR: no arguments supported" >&2
  usage >&2
  exit 2
fi

db_url="$(sed -n 's/^[[:space:]]*database_url[[:space:]]*=[[:space:]]*"\([^"]*\)".*/\1/p' "${CONFIG_FILE}" | head -n1)"
run_objective="$(sed -n 's/^[[:space:]]*objective[[:space:]]*=[[:space:]]*"\([^"]*\)".*/\1/p' "${PROMPT_FILE}" | head -n1)"

if [[ -z "${run_objective}" ]]; then
  echo "YARLI_CONTINUATION_SANITIZE_ERROR: failed to parse objective from ${PROMPT_FILE}" >&2
  exit 1
fi

if [[ ! -f "${CONTINUATION_FILE}" ]]; then
  echo "YARLI_CONTINUATION_SANITIZE_SKIP: missing ${CONTINUATION_FILE}"
  exit 0
fi

lint_output="$("${SCRIPT_DIR}/yarli-lint-implementation-plan.sh" "${PLAN_FILE}")"
open_tranches="$(sed -n 's/.*open_tranches=\([0-9][0-9]*\).*/\1/p' <<< "${lint_output}")"

if [[ -z "${open_tranches}" ]]; then
  echo "YARLI_CONTINUATION_SANITIZE_ERROR: failed to parse open_tranches from lint output" >&2
  echo "${lint_output}" >&2
  exit 1
fi

if [[ "${open_tranches}" != "0" ]]; then
  echo "YARLI_CONTINUATION_SANITIZE_SKIP: open_tranches=${open_tranches}"
  exit 0
fi

file_non_verify_count=0
file_was_cleared=no
if [[ "$(jq -r '.next_tranche == null' "${CONTINUATION_FILE}")" != "true" ]]; then
  file_non_verify_count="$(jq -r '(.next_tranche.config_snapshot.runtime.tranche_plan // []) | map(select(.key != "verification")) | length' "${CONTINUATION_FILE}")"
  tmp_file="$(mktemp)"
  trap 'rm -f "${tmp_file}"' EXIT
  jq '.next_tranche = null' "${CONTINUATION_FILE}" > "${tmp_file}"
  mv "${tmp_file}" "${CONTINUATION_FILE}"
  trap - EXIT
  file_was_cleared=yes
fi

db_reconciled_runs=0
db_cleared_continuations=0
if [[ -n "${db_url}" ]] && command -v psql >/dev/null 2>&1; then
  objective_prefix="${run_objective} [%"
  objective_prefix_sql="${objective_prefix//\'/\'\'}"
  db_reconciled_runs="$(psql "${db_url}" -X -A -q -t -v ON_ERROR_STOP=1 -c "
WITH last_run_event AS (
  SELECT DISTINCT ON (entity_id)
    entity_id::uuid AS run_id,
    event_type
  FROM events
  WHERE entity_type = 'run'
  ORDER BY entity_id, occurred_at DESC, created_at DESC
),
fix AS (
  SELECT
    r.run_id,
    CASE l.event_type
      WHEN 'run.completed' THEN 'RUN_COMPLETED'
      WHEN 'run.cancelled' THEN 'RUN_CANCELLED'
      WHEN 'run.failed' THEN 'RUN_FAILED'
    END AS new_state,
    CASE l.event_type
      WHEN 'run.completed' THEN COALESCE(r.exit_reason, 'completed_all_gates')
      WHEN 'run.cancelled' THEN COALESCE(r.exit_reason, 'cancelled_by_operator')
      WHEN 'run.failed' THEN COALESCE(r.exit_reason, 'failed_runtime_error')
    END AS new_exit
  FROM runs r
  JOIN last_run_event l ON l.run_id = r.run_id
  WHERE r.state = 'RUN_OPEN'
    AND l.event_type IN ('run.completed', 'run.cancelled', 'run.failed')
),
updated AS (
  UPDATE runs r
  SET state = f.new_state,
      exit_reason = f.new_exit,
      updated_at = now()
  FROM fix f
  WHERE r.run_id = f.run_id
  RETURNING 1
)
SELECT count(*) FROM updated;
")"
  db_cleared_continuations="$(psql "${db_url}" -X -A -q -t -v ON_ERROR_STOP=1 -c "
WITH updated AS (
  UPDATE events
  SET payload = jsonb_set(payload, '{continuation_payload,next_tranche}', 'null'::jsonb, true)
  WHERE entity_type = 'run'
    AND event_type = 'run.continuation'
    AND (payload #>> '{continuation_payload,objective}') LIKE '${objective_prefix_sql}'
    AND jsonb_typeof(payload #> '{continuation_payload,next_tranche}') = 'object'
  RETURNING 1
)
SELECT count(*) FROM updated;
")"
fi

echo "YARLI_CONTINUATION_SANITIZE_OK: open_tranches=0 file_cleared=${file_was_cleared} file_previous_non_verify_entries=${file_non_verify_count} db_reconciled_runs=${db_reconciled_runs} db_cleared_continuations=${db_cleared_continuations}"
