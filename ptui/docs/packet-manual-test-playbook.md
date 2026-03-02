# PTUI Packet Manual Test Playbook

This playbook gives us a manual gate before we implement the packet schema +
translator path (`packet -> defwidget/defpanel/defapp`).

## Goal

Protect baseline PTUI behavior while we add description-first packet translation.

## Scope

- Current declarative loader behavior (`ptui.ui.definition-loader`)
- Golden packet fixtures in `ptui/examples/golden-packets/`
- Manual UX checks for project-tree + breadcrumbs style interactions

## Preflight

Run this once before fixture checks:

```bash
./ptui/bin/check-systems.sh
./ptui/bin/build.sh
```

## Golden Packet Set

- `g1-status-strip.lisp`: minimal happy path
- `g2-project-tree-breadcrumbs.lisp`: medium complexity, includes `:widget` + list view + breadcrumb context
- `g3-invalid-unknown-directive.lisp`: intentional failure to validate diagnostics quality

## Manual Checklist

Use this as a release gate during packet-schema and translator implementation.

1. Baseline smoke still passes:
   - `PTUI_EXIT_AFTER_MS=500 ./ptui/dist/metrics-dashboard`
2. G1 loads/runs cleanly:
   - Command exits `0` with no `definition-loader-error`.
   - Header/footer text renders.
3. G2 loads/runs cleanly:
   - Command exits `0` with no `definition-loader-error`.
   - Breadcrumb line shows path context.
   - Tree list renders with a selected row marker.
4. G3 fails for the right reason:
   - `load-definition-file` raises `definition-loader-error`.
   - Error detail mentions unknown declarative directive.
5. Terminal hygiene:
   - After each run, terminal state remains usable (no raw mode leak).

## Run Commands

### G1

```bash
PTUI_EXIT_AFTER_MS=250 sbcl --load ptui/.tools/quicklisp/setup.lisp \
  --eval '(pushnew (truename "./ptui/") asdf:*central-registry*)' \
  --eval '(asdf:load-system :ptui)' \
  --eval '(ptui.ui.definition-loader:load-definition-file
            "ptui/examples/golden-packets/g1-status-strip.lisp")' \
  --eval '(ptui.ui.definition-loader:run-loaded-app
            (find-symbol "STATUS-STRIP-APP"
                         "PTUI.EXAMPLES.GOLDEN.G1-STATUS-STRIP"))' \
  --quit
```

### G2

```bash
PTUI_EXIT_AFTER_MS=250 sbcl --load ptui/.tools/quicklisp/setup.lisp \
  --eval '(pushnew (truename "./ptui/") asdf:*central-registry*)' \
  --eval '(asdf:load-system :ptui)' \
  --eval '(ptui.ui.definition-loader:load-definition-file
            "ptui/examples/golden-packets/g2-project-tree-breadcrumbs.lisp")' \
  --eval '(ptui.ui.definition-loader:run-loaded-app
            (find-symbol "PROJECT-TREE-APP"
                         "PTUI.EXAMPLES.GOLDEN.G2-PROJECT-TREE"))' \
  --quit
```

### G3 (expected failure)

```bash
sbcl --load ptui/.tools/quicklisp/setup.lisp \
  --eval '(pushnew (truename "./ptui/") asdf:*central-registry*)' \
  --eval '(asdf:load-system :ptui)' \
  --eval '(handler-case
              (progn
                (ptui.ui.definition-loader:load-definition-file
                 "ptui/examples/golden-packets/g3-invalid-unknown-directive.lisp")
                (error "Expected failure did not occur."))
            (ptui.ui.definition-loader:definition-loader-error (c)
              (format t "~&EXPECTED_ERROR: ~A~%" c)))' \
  --quit
```

## Translator-Era Acceptance Extensions

When the packet translator lands, add checks for:

1. Generated forms count and shape (expected `defwidget`/`defpanel`/`defapp` set).
2. Stable symbol names for generated panel/widget/app entries.
3. Error spans that point to packet node keys, not only expanded Lisp forms.
