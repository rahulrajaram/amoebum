# Metrics Dashboard Parity Checklist

Updated: 2026-02-11

Goal: migrate to the `ptui/ui` + `ptui/widgets` stack while keeping the legacy dashboard runnable until parity is complete.

## Execution Paths

1. Legacy path (default): `ptui.examples.metrics-dashboard:main-legacy`
2. New path (`PTUI_DASHBOARD_MODE=ui`): `ptui.examples.metrics-dashboard:main-ui`

## Checklist

1. Legacy path still builds and runs under `:backend :auto`: `PASS`
2. UI/widgets path builds and runs under `:backend :auto`: `PASS`
3. Legacy and UI paths are selectable from one binary: `PASS`
4. UI path shows title, terminal info, quit hint, input row, gradient row, and status row: `PASS`
5. UI path accepts focused text input and backspace edits on the input row: `PASS`
6. UI path keeps `q` / `Ctrl-C` quit semantics through engine loop: `PASS`
7. Full verification tranche passes (`check-systems`, `test`, `build`, `compliance-gate`): `PASS`

## Notes

1. Legacy remains the default execution mode to preserve existing behavior while parity hardens.
2. UI mode currently uses runtime reconciliation + layout contracts to render widgets; this is the migration foundation for full replacement.
