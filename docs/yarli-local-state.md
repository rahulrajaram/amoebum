# Yarli Local State

`.yarli/` is machine-local operator state for this repo.

Tracked project truth lives in:

1. `yarli.toml` for runtime behavior.
2. `PROMPT.md` for intent and current operating focus.
3. `IMPLEMENTATION_PLAN.md` for durable tranche scope, historical evidence, and closure criteria.

Local-only state lives in:

1. `.yarli/tranches.toml` for the current operator queue.
2. `.yarli/evidence/` for local logs and rerun artifacts.
3. `.yarli/run-risk-audit.json` for the latest continuation risk classification.
4. Other `.yarli/*` files created by Yarli during execution.

Bootstrap steps:

1. Run `./bin/yarli-bootstrap-local-state.sh` if `.yarli/tranches.toml` is missing.
2. Run `make yarli-bootstrap-validate` if you want the bootstrap + validate flow in one command.
3. Run `./bin/yarli-run-risk-audit.sh` before resuming a token-heavy continuation so local token history, file pressure, and recent kill/OOM symptoms are visible.
4. Run `./bin/yarli-local-state-regression.sh` to exercise the missing-tranches and repo-wrapper help-path smoke checks.
5. Add or complete local tranches as needed for the current shell.

Working rule:

Do not treat `.yarli/` as a committed source-of-truth artifact. If the local queue is deleted, recreate it from the tracked docs above.
