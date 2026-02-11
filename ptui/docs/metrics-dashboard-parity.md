# Metrics Dashboard Parity Checklist

Updated: 2026-02-11

Goal: migrate to the `ptui/ui` + `ptui/widgets` stack while keeping the legacy dashboard runnable until parity is complete.

## Execution Paths

1. UI path (default): `ptui.examples.metrics-dashboard:main-ui`
2. Legacy path (`PTUI_DASHBOARD_MODE=legacy`): `ptui.examples.metrics-dashboard:main-legacy`

## Checklist

1. UI/widgets path still builds and runs under `:backend :auto` as default: `PASS`
2. Legacy path still builds and runs under `:backend :auto` via compat mode: `PASS`
3. Legacy and UI paths are selectable from one binary: `PASS`
4. UI path shows title, terminal info, quit hint, input row, gradient row, and status row: `PASS`
5. UI path accepts focused text input and backspace edits on the input row: `PASS`
6. UI path keeps `q` / `Ctrl-C` quit semantics through engine loop: `PASS`
7. Full verification tranche passes (`check-systems`, `test`, `build`, `compliance-gate`): `PASS`
8. Automated parity tests assert line-presence/width/input signals across legacy and UI paths: `PASS`

## Notes

1. UI is now the default execution mode; legacy remains available through env override during transition.
2. UI mode renders box/spacer/scroll widgets through layout bounds + clipping and is the migration foundation for full replacement.
