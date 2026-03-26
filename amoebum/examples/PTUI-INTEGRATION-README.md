# PTUI Integration for Amoebum

This directory contains examples and documentation for the PTUI (Programmable Terminal UI) features integrated into amoebum.

## What Was Borrowed from PTUI

### 1. Scroll Utilities (`src/ui/scroll-util.lisp`)
Borrowed from: `ptui/src/util/scroll.lisp`

- **Bottom-origin scroll semantics**: Offset 0 = live tail
- **Key-to-scroll mapping**: Unified handling of arrows, pgup/pgdn, home/end
- **Scroll clamping**: Keeps scroll offset within valid bounds
- **Debug logging**: Runtime toggle via `AMOEBUM_SCROLL_DEBUG` env var

### 2. YAML Layout System (`src/ui/layout-yaml.lisp`)
Borrowed from: `ptui/src/preview/yaml-translator.lisp`

- **Declarative layouts**: Define UI structure in YAML
- **Constraint-based sizing**: `fill`, `content`, or fixed pixel heights
- **Named regions**: `history`, `prompt`, `status` panels
- **Scroll configuration**: `scroll-follow`, `scroll-bar`, `overflow-y`

### 3. Enhanced Theme Loader (`src/ui/yaml-theme-layout.lisp`)
Borrowed from: `ptui/src/preview/preview-app.lisp`

- **Layout loading**: Parse `layout:` section from YAML
- **Behavior config**: `scroll`, `input`, `notifications`, `keys`
- **Hot reload**: File watching with `reload-yaml-theme-if-changed`

## Files

| File | Description |
|------|-------------|
| `ptui-theme-demo.yaml` | Example theme with palette, roles, layout, behavior |
| `ptui-integration-demo.lisp` | Interactive demo of new features |

## Usage

### Load a Theme with Layout

```bash
# Command line
amoebum --theme-yaml examples/ptui-theme-demo.yaml

# Or auto-detect from standard locations:
#   ~/.config/amoebum/theme.yaml
#   ~/.amoebum/theme.yaml
#   ./.amoebum/theme.yaml
```

### Theme YAML Structure

```yaml
palette:
  bg: "16 18 24"
  text: "220 226 236"
  amber: "224 175 104"

roles:
  user-label:
    fg: amber
    bold: true

layout:
  children:
    - name: history
      height: fill
      scroll-follow: true
    - name: prompt
      height: 3
      border: single

behavior:
  scroll:
    page-step: 10
  keys:
    reload: "r"
    quit: "q"
```

## Interactive Features

- **Hot Reload**: Edit the YAML while amoebum is running, press `r` to reload
- **Debug Logging**: `AMOEBUM_SCROLL_DEBUG=1 amoebum` enables scroll debug output
- **Key Scrolling**: Use arrows, pgup/pgdn, home/end for history navigation

## Implementation Files

Located in `amoebum/src/ui/`:
- `scroll-util.lisp` - Scroll utilities
- `layout-yaml.lisp` - Layout parsing
- `yaml-theme-layout.lisp` - Theme + layout integration
- `yaml-theme-loader.lisp` - Base theme loader (existing)
