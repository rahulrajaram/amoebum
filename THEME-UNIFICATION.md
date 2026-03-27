# Amoebum Theme Unification: YAML as Default

## Summary

Successfully implemented Phase 1 of the PTUI/amoebum theme unification:
- Extracted Tokyo Night theme to YAML format
- Made YAML theme loading the default (with Lisp fallback)
- Added comprehensive test coverage

## Changes Made

### 1. New Theme File: `amoebum/resources/themes/amoebum.tui-spec.yaml`
- Complete Tokyo Night theme specification
- 19 palette colors
- 45 role definitions
- Metadata section (name, version, description, base theme)
- Layout configuration (panel structure)
- Behavior configuration (keys, scroll, input)

### 2. Updated `amoebum/src/ui/yaml-theme-loader.lisp`
- Added `%yaml-theme-parse-metadata` function
- Enhanced `%yaml-theme-apply` to use metadata.name and metadata.base
- Fixed file reading to use `uiop:read-file-string` + `cl-yaml:parse`
- Added `install-default-yaml-theme` function for easy installation
- Improved logging with metadata details

### 3. Updated `amoebum/src/main.lisp`
- Added YAML theme loading at startup (before chat UI runs)
- Falls back to Lisp theme if no YAML theme found
- Logs which theme source is being used

### 4. Added Test: `amoebum/test/yaml-theme-smoke-test.lisp`
- Validates YAML syntax
- Checks all required sections (metadata, palette, roles)
- Verifies theme file exists
- Registered in `amoebum.asd`

## Usage

### Auto-detection (Current Default)
Amoebum now automatically looks for YAML themes in this order:
1. CLI: `--theme-yaml <path>`
2. Project: `<project-root>/.amoebum/theme.yaml`
3. Global: `~/.config/amoebum/theme.yaml`
4. Legacy: Built-in Lisp theme (fallback)

### Install Default Theme
```bash
# In Lisp REPL
(amoebum.ui:install-default-yaml-theme)
```

### Manual Copy
```bash
mkdir -p ~/.config/amoebum
cp amoebum/resources/themes/amoebum.tui-spec.yaml ~/.config/amoebum/theme.yaml
```

## Verification

Test passes:
```bash
cd amoebum
sbcl --load test/yaml-theme-smoke-test.lisp
```

## Next Steps (Phase 2)

1. **Content Provider Protocol**: Connect YAML layout to live amoebum state
2. **Conditional Visibility**: Map YAML `when:` clauses to amoebum predicates
3. **Event Binding**: Connect YAML key bindings to handlers
4. **Hot Reload**: Real-time theme updates during development
5. **Eventually Deprecate**: Remove `theme-amoebum.lisp` once YAML path is proven

## Benefits

- **Theme authors**: Edit YAML instead of Lisp code
- **Hot reload**: File watching for instant updates
- **Version control**: Themes as data, not code
- **Ecosystem**: Share themes with PTUI-compatible tools
- **Flexibility**: Multiple themes, easy switching
