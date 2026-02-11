# PTUI Text + Layout API Notes

This document is intentionally short and focused on consumable boundaries.

## `ptui/text`

Load only text functionality:

```lisp
(asdf:load-system "ptui/text")
```

Primary packages:

- `ptui.text.grapheme`
- `ptui.text.width`
- `ptui.text.layout`
- `ptui.text.engine`

Core entry points:

- `ptui.text.grapheme:split-graphemes`
- `ptui.text.width:string-width`
- `ptui.text.layout:wrap-by-width`
- `ptui.text.layout:truncate-to-width`
- `ptui.text.engine:resolve-text-engine`

Engine model:

- `:fallback` is deterministic and always available.
- `:native` is an explicit adapter slot and currently unavailable by default.
- `:auto` resolves to `:native` when available, else `:fallback`.

## `ptui/layout`

Load only layout contracts + deterministic solver:

```lisp
(asdf:load-system "ptui/layout")
```

Primary package:

- `ptui.layout`

Core data contracts:

- `layout-size` (`width`, `height`)
- `layout-bounds` (`x`, `y`, `width`, `height`)
- `layout-node` (`id`, `direction`, `width`, `height`, `gap`, `children`, `measure`)

Core entry points:

- `ptui.layout:make-layout-node`
- `ptui.layout:compute-layout`
- `ptui.layout:layout-bound`
- `ptui.layout:layout->alist`

Measure contract:

- `measure` is a function `(lambda (available-width available-height) => layout-size)`.
- Return `layout-size`; do not mutate layout state.

## Optional `ptui/layout/yoga`

The Yoga boundary adapter is feature-gated:

```lisp
(pushnew :ptui-layout-yoga *features*)
(asdf:load-system "ptui/layout/yoga")
```

Current status:

- Adapter package: `ptui.layout.yoga`
- Entry point: `ptui.layout.yoga:compute-layout`
- Behavior today delegates to `ptui.layout:compute-layout` until native Yoga bindings are added.
