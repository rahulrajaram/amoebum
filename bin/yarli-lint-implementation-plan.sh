#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
PLAN_FILE="${1:-${REPO_ROOT}/IMPLEMENTATION_PLAN.md}"

if [[ ! -f "${PLAN_FILE}" ]]; then
  echo "PLAN_LINT_ERROR: missing plan file: ${PLAN_FILE}" >&2
  exit 2
fi

awk '
BEGIN {
  in_next_work = 0
  in_stanza = 0
  seen_next_work = 0
  errors = 0
  open_tranches = 0
}

function fail(msg, line_no) {
  errors += 1
  printf("PLAN_LINT_ERROR: line %d: %s\n", line_no, msg) > "/dev/stderr"
}

function close_stanza(line_no) {
  if (!in_stanza) {
    return
  }

  if (!have_scope) {
    fail("missing Scope: section for " tranche_id, line_no)
  } else if (scope_items < 1) {
    fail("Scope: has no numbered entries for " tranche_id, line_no)
  }

  if (!have_exit) {
    fail("missing Exit criteria: section for " tranche_id, line_no)
  } else if (exit_items < 1) {
    fail("Exit criteria: has no numbered entries for " tranche_id, line_no)
  }

  if (status == "complete") {
    if (!have_evidence) {
      fail("missing Verification evidence: section for " tranche_id, line_no)
    } else if (evidence_items < 1 && !have_evidence_ref) {
      fail("Verification evidence: has no numbered entries for " tranche_id, line_no)
    }
  }

  if (status == "incomplete" || status == "blocked") {
    open_tranches += 1
  }

  in_stanza = 0
  tranche_id = ""
  status = ""
  section = ""
  have_scope = 0
  have_exit = 0
  have_evidence = 0
  have_evidence_ref = 0
  scope_items = 0
  exit_items = 0
  evidence_items = 0
}

/^## Next Work Tranches$/ {
  in_next_work = 1
  seen_next_work = 1
  next
}

in_next_work && /^Operator policy while queue is non-empty:/ {
  close_stanza(NR)
  in_next_work = 0
  next
}

!in_next_work {
  next
}

{
  line = $0

  if (line ~ /^[0-9]+\. I[0-9A-Z]+ `[^`]+`: (incomplete|blocked|complete)\. tranche_group=[a-z0-9][a-z0-9-]*$/) {
    close_stanza(NR)

    split(line, header_parts, ": ")
    split(header_parts[1], left_tokens, " ")
    tranche_id = left_tokens[2]

    split(header_parts[2], right_tokens, ". tranche_group=")
    status = right_tokens[1]

    in_stanza = 1
    section = ""
    have_scope = 0
    have_exit = 0
    have_evidence = 0
    have_evidence_ref = 0
    scope_items = 0
    exit_items = 0
    evidence_items = 0
    if (seen_ids[tranche_id] == 1) {
      fail("duplicate tranche id in Next Work Tranches: " tranche_id, NR)
    }
    seen_ids[tranche_id] = 1
    next
  }

  if (match(line, /^[0-9]+\. I[0-9A-Z]+ /)) {
    fail("invalid tranche header format; expected <n>. I<id> `<title>`: <status>. tranche_group=<group>", NR)
    next
  }

  if (!in_stanza) {
    if (line ~ /^[[:space:]]*$/) {
      next
    }
    fail("content appears outside a tranche stanza in Next Work Tranches", NR)
    next
  }

  if (line ~ /^    Scope:$/) {
    if (have_scope) {
      fail("duplicate Scope: section for " tranche_id, NR)
    }
    if (have_exit) {
      fail("Scope: appears after Exit criteria: in " tranche_id, NR)
    }
    have_scope = 1
    section = "scope"
    next
  }

  if (line ~ /^    Exit criteria:$/) {
    if (!have_scope) {
      fail("Exit criteria: appears before Scope: in " tranche_id, NR)
    }
    if (have_exit) {
      fail("duplicate Exit criteria: section for " tranche_id, NR)
    }
    have_exit = 1
    section = "exit"
    next
  }

  if (line ~ /^    Verification evidence:/) {
    if (!have_exit) {
      fail("Verification evidence: appears before Exit criteria: in " tranche_id, NR)
    }
    if (have_evidence) {
      fail("duplicate Verification evidence: section for " tranche_id, NR)
    }
    have_evidence = 1
    section = "evidence"
    if (line ~ /see \.yarli\/evidence\//) {
      have_evidence_ref = 1
    }
    next
  }

  if (line ~ /^    [0-9]+\. /) {
    if (section == "scope") {
      scope_items += 1
      next
    }
    if (section == "exit") {
      exit_items += 1
      next
    }
    if (section == "evidence") {
      evidence_items += 1
      next
    }
    fail("numbered list entry appears before Scope:/Exit criteria: in " tranche_id, NR)
    next
  }

  if (line ~ /^    [[:alpha:]][[:alnum:] _-]*:/) {
    fail("unknown subsection in tranche stanza " tranche_id ": " line, NR)
    next
  }

  if (line ~ /^[[:space:]]*$/) {
    next
  }

  fail("unexpected line inside tranche stanza " tranche_id ": " line, NR)
}

END {
  close_stanza(NR)

  if (!seen_next_work) {
    fail("missing section header: ## Next Work Tranches", NR)
  }

  if (errors > 0) {
    exit 1
  }

  printf("PLAN_LINT_OK: file=%s open_tranches=%d\n", ARGV[1], open_tranches)
}
' "${PLAN_FILE}"
