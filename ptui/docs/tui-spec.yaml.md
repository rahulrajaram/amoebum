# `.tui-spec.yaml` Format Specification

**Version:** 1.0
**Status:** Normative
**Framework:** ptui (Common Lisp terminal UI)

---

## Overview

A `.tui-spec.yaml` file is the canonical description of a ptui application's visual structure. It declares three things: how panels are arranged on screen (layout), what colors exist (palette), and what semantic style roles map to those colors (roles).

A translator — human or automated — converts the spec into ptui's `define-theme` macro call and the widget tree configuration. The spec is intentionally declarative and framework-neutral at the description level; it contains no Lisp, no widget class names, and no event bindings.

### Target Runtime

ptui renders to a cell-based terminal grid. A **cell** is one character position carrying:

- A foreground color (RGB)
- A background color (RGB or transparent)
- An attribute set: bold, dim, italic, underline, inverse, strikethrough

All sizing in this spec is expressed in cells.

### File Format

| Property | Value |
|----------|-------|
| File extension | `.tui-spec.yaml` |
| Encoding | UTF-8 |
| YAML version | 1.2 |
| Required top-level keys | `layout`, `palette`, `roles` |

Processing order is fixed: `palette` is resolved first (defines all named colors), then `roles` (references palette names), then `layout` (may reference role names for border styling or background fills).

---

## Type Definitions

The following primitive types are used throughout this specification.

| Type | Definition |
|------|-----------|
| `Cell` | Non-negative integer. Represents a count of character positions (columns) or rows. |
| `Size` | `Cell \| "fill" \| "content"`. Fixed cell count, remaining available space, or shrink-to-content. |
| `Inset` | `Cell \| [vertical, horizontal] \| [top, right, bottom, left]`. Shorthand expansion follows CSS conventions: one value applies to all sides; two values apply to vertical then horizontal; four values apply to top, right, bottom, left in order. All component values must be non-negative integers. |
| `BorderStyle` | `"none" \| "single" \| "double" \| "rounded" \| "heavy" \| "ascii"` |
| `OverflowBehavior` | `"truncate" \| "scroll" \| "wrap" \| "visible"` |
| `Align` | Horizontal: `"left" \| "center" \| "right"`. Vertical: `"top" \| "center" \| "bottom"`. |
| `ChildAlign` | `"start" \| "center" \| "end" \| "stretch"` |
| `ScrollBar` | `"auto" \| "always" \| "never"` |
| `Anchor` | `"center" \| "top" \| "bottom" \| "cursor"` |
| `WrapMode` | `"word" \| "char" \| "none"` |
| `Color` | `"R G B"` (space-separated integers, each 0–255) or `"#RRGGBB"` (six-digit hex). Both forms are valid in the palette section. |
| `AttrSet` | A combination of boolean keys: `bold`, `dim`, `italic`, `underline`, `inverse`, `strike`. Each defaults to `false` when omitted. |

---

## Section 1: `layout`

The `layout` section describes the panel tree. The root of the tree is an implicit vertical stack. Panels are rendered in list order, top to bottom. Nesting a node with `children` and `direction: horizontal` creates a horizontal split at that level.

Overlay nodes (panels with `z-index > 0`) are declared under the top-level `overlays` key and are rendered above the normal stacking order.

### 1.1 Layout Node Properties

A layout node is a YAML mapping. The 28 properties are organized into nine categories.

---

#### Identity

| Property | Type | Default | Description |
|----------|------|---------|-------------|
| `name` | string | **required** | Unique identifier for this panel across the entire tree. Used for focus targeting, scroll commands, and content binding by the application layer. |
| `visible` | bool | `true` | When `false`, this node and all its children are removed from layout entirely (collapsed, not hidden in place). No space is reserved. |

---

#### Structure

| Property | Type | Default | Description |
|----------|------|---------|-------------|
| `direction` | `"vertical" \| "horizontal"` | `"vertical"` | The axis along which children are stacked. `"vertical"` = top-to-bottom; `"horizontal"` = left-to-right. |
| `children` | list[node] | `[]` | Nested layout nodes rendered in list order along the `direction` axis. Each child is a full layout node and may itself have children. |

---

#### Sizing

| Property | Type | Default | Description |
|----------|------|---------|-------------|
| `width` | Size | `"fill"` | Horizontal size. `"fill"` claims all remaining space in the parent container. `"content"` shrinks to the width of rendered content. Fixed integer = exact column count. |
| `height` | Size | `"content"` | Vertical size. Same semantics as `width` applied to the vertical axis. |
| `min-width` | Cell | `0` | Hard floor. The layout solver will never reduce this node's width below this value, even under space pressure. |
| `max-width` | Cell | `null` (unbounded) | Hard ceiling on width. Content that exceeds this is subject to `overflow-x` behavior. |
| `min-height` | Cell | `0` | Hard floor for vertical size. |
| `max-height` | Cell | `null` (unbounded) | Hard ceiling on height. Content that exceeds this triggers `overflow-y` behavior. |
| `height-cap` | Cell | `null` | Soft vertical cap. Renders up to N rows of content, then replaces remaining content with a `"{n} more lines"` affordance. Distinct from `max-height`: `max-height` hard-clips without indication; `height-cap` shows a count. Valid only when `overflow-y` is `"scroll"` or `"truncate"`. |
| `fill-weight` | float | `1.0` | When multiple siblings all use `"fill"` for the same axis, available space is distributed proportionally by `fill-weight`. A node with weight `2.0` receives twice the space of a node with weight `1.0`. Ignored when the node's size is not `"fill"`. |

---

#### Spacing

| Property | Type | Default | Description |
|----------|------|---------|-------------|
| `padding` | Inset | `0` | Internal space between the border (or node edge, if no border) and the content area. Follows Inset shorthand rules. |
| `margin` | Inset | `0` | External space outside the border, between this node and adjacent siblings or the parent edge. Follows Inset shorthand rules. |
| `gap` | Cell | `0` | Additional space inserted between adjacent children along the stack axis. Applied between children only, not before the first or after the last. |
| `gutter` | `[left, right] \| Cell` | `0` | Reserved columns on the left and/or right edges of the content area, inside padding. Used for line numbers, scroll position indicators, or decorative vertical rules. A single integer value applies to the left edge only. Gutter cells are subtracted from available content width before layout. |

---

#### Borders

| Property | Type | Default | Description |
|----------|------|---------|-------------|
| `border` | BorderStyle | `"none"` | Box-drawing character set for the border. `"single"` uses `─│┌┐└┘`; `"double"` uses `═║╔╗╚╝`; `"rounded"` uses `─│╭╮╰╯`; `"heavy"` uses `━┃┏┓┗┛`; `"ascii"` uses `+-\|`; `"none"` draws no border. |
| `border-sides` | list[`"top" \| "right" \| "bottom" \| "left"`] | `["all"]` | Which edges of the border to draw. The special value `["all"]` means all four sides. Any subset may be specified. |
| `title` | string | `null` | A label string rendered inline within the top border segment. Valid only when `border` is not `"none"` and `border-sides` includes `"top"`. |
| `title-align` | `"left" \| "center" \| "right"` | `"left"` | Horizontal position of the title string within the top border. |

---

#### Overflow

| Property | Type | Default | Description |
|----------|------|---------|-------------|
| `overflow-x` | OverflowBehavior | `"truncate"` | Behavior when content width exceeds the node's allocated width. `"truncate"` clips and appends `truncation-marker`. `"scroll"` creates a horizontal scroll region (uncommon in TUIs). `"wrap"` soft-wraps lines (controlled by `wrap`). `"visible"` allows content to paint outside the node boundary (overlays only). |
| `overflow-y` | OverflowBehavior | `"truncate"` | Behavior when content height exceeds the node's allocated height. `"truncate"` clips. `"scroll"` creates a scrollable region. `"visible"` is valid only for overlay nodes (`z-index > 0`). |
| `wrap` | WrapMode | `"word"` | Text wrap strategy when `overflow-x` is `"wrap"`. `"word"` breaks at word boundaries (space, hyphen). `"char"` breaks at any character. `"none"` disables wrapping (equivalent to `overflow-x: "truncate"`). |
| `truncation-marker` | string | `"…"` | The character or string appended at the right edge of a line when horizontal truncation occurs. May be set to `""` to truncate silently. |

---

#### Scroll

| Property | Type | Default | Description |
|----------|------|---------|-------------|
| `scroll-follow` | bool | `false` | When `true`, the scroll position automatically tracks the bottom of the content as new content is appended (sticky-bottom behavior). Disengages automatically when the user scrolls up manually; re-engages when the user scrolls back to the bottom. Valid only when `overflow-y` is `"scroll"`. |
| `scroll-bar` | ScrollBar | `"auto"` | Controls visibility of the scroll position indicator on the right edge of the node. `"auto"` shows the indicator only when the content is scrolled away from the default position. `"always"` shows it unconditionally. `"never"` suppresses it entirely. |

---

#### Alignment

| Property | Type | Default | Description |
|----------|------|---------|-------------|
| `align-x` | Align (horizontal) | `"left"` | Horizontal alignment of this node's rendered content within its allocated content area. |
| `align-y` | Align (vertical) | `"top"` | Vertical alignment of this node's rendered content within its allocated content area. |
| `align-children` | ChildAlign | `"stretch"` | Cross-axis alignment of children. In a horizontal stack, this controls vertical alignment of each child. In a vertical stack, this controls horizontal alignment. `"stretch"` causes each child to fill the cross axis. |

---

#### Focus

| Property | Type | Default | Description |
|----------|------|---------|-------------|
| `focusable` | bool | `false` | Whether this node can receive keyboard focus via tab navigation or explicit focus targeting. |
| `focus-order` | int | `0` | Tab-order index. Focus cycles through focusable nodes in ascending order. Ties are broken by document order (depth-first traversal). |
| `captures-input` | bool | `false` | When this node has focus, all key events are consumed by this node and do not bubble to parent nodes or the application key handler. Required for modal overlays and inline editors. Requires `focusable: true`. |

---

#### Overlay

| Property | Type | Default | Description |
|----------|------|---------|-------------|
| `z-index` | int | `0` | Stacking order for nodes that overlap. Nodes with higher `z-index` are rendered on top. Nodes with `z-index > 0` are treated as overlays and declared under the `overlays` key. |
| `anchor` | Anchor | `"center"` | Positioning origin for overlay nodes. `"center"` centers the overlay within the parent viewport. `"top"` pins to the top edge. `"bottom"` pins to the bottom edge. `"cursor"` positions relative to the current cursor/insertion point. Meaningful only when `z-index > 0`. |

---

### 1.2 `BorderStyle` Reference

| Value | Box-drawing characters used | Notes |
|-------|-----------------------------|-------|
| `"none"` | (none) | Default; no border drawn |
| `"single"` | `─ │ ┌ ┐ └ ┘` | Standard single-line box |
| `"double"` | `═ ║ ╔ ╗ ╚ ╝` | Double-line box |
| `"rounded"` | `─ │ ╭ ╮ ╰ ╯` | Single lines with rounded corners |
| `"heavy"` | `━ ┃ ┏ ┓ ┗ ┛` | Thick single-line box |
| `"ascii"` | `- + \| +` | Pure ASCII fallback for limited terminals |

All box-drawing characters require a terminal font with Unicode line-drawing support. Use `"ascii"` as a safe fallback.

---

### 1.3 Size Resolution

When the layout solver allocates space to children, it applies the following order of operations:

1. Assign fixed-size children (`width`/`height` is a `Cell` integer) their exact allocation.
2. Assign `"content"` children the minimum space required to render their content without overflow.
3. Sum the remaining space after fixed and content children.
4. Distribute remaining space among `"fill"` children proportionally by `fill-weight`.
5. Apply `min-width`/`min-height` floors and `max-width`/`max-height` ceilings.

If a `"fill"` child's computed allocation would fall below its `min-width`/`min-height`, the floor is honored and the deficit is taken from other `"fill"` siblings in weight order.

---

## Section 2: `palette`

The `palette` section defines all named colors available to the `roles` section. It is a flat mapping from name to color value.

### 2.1 Syntax

```yaml
palette:
  <name>: "<R> <G> <B>"     # space-separated integers, each 0–255
  <name>: "#RRGGBB"          # six-digit hex string
```

Both forms may be mixed within a single palette.

### 2.2 Rules

1. Names must match the pattern `[a-z][a-z0-9-]*` (lowercase alphanumeric, hyphens allowed, must start with a letter).
2. Space-separated form: exactly three integers separated by single spaces, each in the range 0–255.
3. Hex form: exactly six hex digits after the `#` prefix. Three-digit shorthand (`#RGB`) is not valid.
4. Names must be unique within the palette block.
5. All palette names referenced in `roles` must be defined in `palette`. Forward references within the palette block itself are not applicable (palette is a flat mapping, not ordered).

### 2.3 Example

```yaml
palette:
  bg:        "16 18 24"
  surface:   "30 36 46"
  text:      "220 226 236"
  muted:     "160 170 186"
  blue:      "#7aa2f7"
  rose:      "242 143 173"
```

---

## Section 3: `roles`

The `roles` section maps semantic names to concrete visual styles. Each role specifies a foreground color (required), an optional background color, and optional text attributes.

### 3.1 Syntax

```yaml
roles:
  <role-name>:
    fg: <palette-name>
    bg: <palette-name>       # optional; omit for transparent
    bold: true               # optional attribute
    dim: true                # optional attribute
    italic: true             # optional attribute
    underline: true          # optional attribute
    inverse: true            # optional attribute
    strike: true             # optional attribute
```

Inline mapping form is also valid and preferred for compact definitions:

```yaml
roles:
  heading: { fg: blue, bold: true }
  error:   { fg: rose, bold: true }
```

### 3.2 Properties

| Property | Type | Required | Description |
|----------|------|----------|-------------|
| `fg` | palette name | **required** | Foreground (text) color. Must reference a name defined in `palette`. |
| `bg` | palette name | optional | Background fill color. When omitted, the role is transparent — the terminal background or parent panel background shows through. |
| `bold` | bool | optional | Render text with bold weight. |
| `dim` | bool | optional | Render text at reduced intensity. |
| `italic` | bool | optional | Render text in italic style. |
| `underline` | bool | optional | Render text with underline. |
| `inverse` | bool | optional | Swap foreground and background colors. |
| `strike` | bool | optional | Render text with strikethrough. |

Attribute keys default to `false` when omitted. There is no conflict rule between attributes; a role may combine multiple attributes (e.g., `bold: true, italic: true`).

### 3.3 Rules

1. Role names must match the pattern `[a-z][a-z0-9-]*`.
2. `fg` is required on every role; a role with no `fg` is a validation error.
3. Every `fg` and `bg` value must reference a name defined in the `palette` section. Referencing an undefined palette name is a validation error.
4. Role names must be unique within the `roles` block.
5. `dim` and `bold` may coexist; rendering behavior for this combination is terminal-dependent.

---

## Validation Rules

The following rules must all pass for a spec file to be considered valid. A conforming translator must reject files that fail any rule.

| # | Rule |
|---|------|
| 1 | Every `name` value in the layout tree must be unique across the entire tree (including nodes inside `overlays`). |
| 2 | Every palette name referenced by `fg` or `bg` in `roles` must be defined in `palette`. |
| 3 | In a vertical stack, at most one child may use `height: "fill"` — unless `fill-weight` is used to distribute space across multiple `"fill"` children. |
| 4 | In a horizontal stack, at most one child may use `width: "fill"` — unless `fill-weight` is used to distribute across multiple `"fill"` children. |
| 5 | `height-cap` is only valid when `overflow-y` is `"scroll"` or `"truncate"`. |
| 6 | `title` is only valid when `border` is not `"none"` and `border-sides` includes `"top"` (or is `["all"]`). |
| 7 | `scroll-follow` is only valid when `overflow-y` is `"scroll"`. |
| 8 | `captures-input: true` requires `focusable: true` on the same node. |
| 9 | `anchor` is only meaningful on nodes with `z-index > 0`. A validator may warn (not error) when `anchor` is set on a non-overlay node. |
| 10 | `gutter` cells are subtracted from the available content width of the node. The resulting content width must be non-negative; a `gutter` wider than the available content area is a validation error. |
| 11 | `fill-weight` is ignored (and a validator may warn) when the node's `width` (for horizontal stacks) or `height` (for vertical stacks) is not `"fill"`. |
| 12 | All component values of an `Inset` must be non-negative integers. Negative padding or margin is a validation error. |

---

## Complete Example

The following file is the canonical amoebum layout spec. It demonstrates all three sections and most property categories.

```yaml
# amoebum.tui-spec.yaml
#
# Layout spec for the amoebum TUI: a vertically stacked chat interface
# with a scrollable history, a single-row status bar, a bordered prompt
# input area, and a centered modal approval overlay.

layout:
  children:
    - name: history
      height: fill
      overflow-y: scroll
      scroll-follow: true
      scroll-bar: auto
      gutter: 1
      padding: [0, 1]

    - name: status
      height: 1
      direction: horizontal
      children:
        - name: context-pct
          width: 12
        - name: mode
          width: fill
        - name: activity
          width: content

    - name: prompt
      height: 3
      border: single
      focusable: true
      focus-order: 1

  overlays:
    - name: approval
      anchor: center
      z-index: 1
      border: single
      width: 60
      height: content
      max-height: 20
      focusable: true
      captures-input: true

palette:
  bg:        "16 18 24"
  surface:   "30 36 46"
  code-bg:   "36 42 54"
  border:    "58 70 86"
  meta:      "116 124 138"
  text:      "220 226 236"
  muted:     "160 170 186"
  amber:     "224 175 104"
  blue:      "122 162 247"
  cyan:      "125 207 255"
  green:     "158 206 106"
  yellow:    "240 198 116"
  rose:      "242 143 173"
  steel:     "109 140 189"

roles:
  user-label:       { fg: amber,  bold: true }
  assistant-label:  { fg: blue,   bold: true }
  tool-label:       { fg: cyan,   bold: true }
  user:             { fg: text }
  assistant:        { fg: text }
  tool:             { fg: cyan }
  system:           { fg: muted }
  heading:          { fg: blue,   bold: true }
  inline-code:      { fg: text,   bg: code-bg }
  code-body:        { fg: text }
  code-fence:       { fg: border, dim: true }
  code-keyword:     { fg: yellow, bold: true }
  status-bar:       { fg: muted,  bg: surface }
  context-ok:       { fg: green,  bg: surface, bold: true }
  context-warn:     { fg: yellow, bg: surface, bold: true }
  context-danger:   { fg: rose,   bg: surface, bold: true }
  prompt-border:    { fg: steel }
  warning:          { fg: yellow, bold: true }
  error:            { fg: rose,   bold: true }
  meta:             { fg: meta }
```

### Example: Multiple `fill` Children with `fill-weight`

```yaml
layout:
  direction: horizontal
  children:
    - name: sidebar
      width: fill
      fill-weight: 1.0     # receives 1/4 of remaining space
    - name: main
      width: fill
      fill-weight: 3.0     # receives 3/4 of remaining space
```

### Example: Inset Shorthand Forms

```yaml
# All four forms are equivalent in effect for uniform spacing:
padding: 2                  # all sides = 2
padding: [2, 2]             # vertical=2, horizontal=2
padding: [2, 2, 2, 2]       # top=2, right=2, bottom=2, left=2

# Asymmetric padding:
padding: [0, 1]             # top/bottom=0, left/right=1
padding: [1, 2, 0, 2]       # top=1, right=2, bottom=0, left=2
```

### Example: Bordered Panel with Title

```yaml
- name: tool-output
  border: rounded
  border-sides: ["top", "bottom"]
  title: "Tool Output"
  title-align: center
  height: content
  max-height: 10
  overflow-y: scroll
```

---

## Translator Notes

When converting a `.tui-spec.yaml` file to ptui's `define-theme` macro and widget tree, the following mappings apply:

- Each `palette` entry becomes a color constant in the theme.
- Each `roles` entry becomes a `define-style` entry binding a foreground cell attribute, optional background cell attribute, and the specified `AttrSet`.
- Each `layout` node becomes a widget configuration entry. The `name` becomes the widget identifier used for `bind-content` calls at runtime.
- `overlays` nodes are registered in the overlay layer of the root viewport widget with their `z-index` and `anchor` settings.
- `gutter` values are passed as left-margin and right-margin parameters to the text renderer for the node.
- `scroll-follow` maps to the `:auto-scroll` parameter on scrollable viewport widgets.
- `captures-input` maps to the `:modal` parameter on focusable widgets.

The spec intentionally omits all application-level bindings (which content populates which panel, what keystrokes trigger which actions). Those are runtime concerns handled by the ptui application layer, not the layout spec.
