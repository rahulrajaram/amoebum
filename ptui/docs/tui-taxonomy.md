# TUI Taxonomy

Reference for terminal UI concepts as they apply to ptui and amoebum.

---

## Spatial Concepts

**Viewport**
The visible rectangle of the terminal. Everything you see at once. Defined
by columns (width) and rows (height). Content outside the viewport exists
but is not drawn until you scroll.

**Canvas**
The full logical surface that content is laid out on. Can be taller than
the viewport (scrollable) or wider (horizontally scrollable, rare in TUIs).

**Panel**
A rectangular region of the viewport with its own content and behavior.
Panels subdivide the viewport. In amoebum: message history panel, prompt
panel, status bar panel.

**Pane**
Synonym for panel in some frameworks. In ptui, "panel" is the canonical
term (see `defpanel`).

**Region / Zone**
An informal subdivision within a panel. Not a first-class layout object,
just a conceptual area. "The code block region" or "the role label zone."

---

## Layout Concepts

**Flow**
Content laid out sequentially — each item appears after the previous one.
Chat messages are in a vertical flow. Text within a message is in a
horizontal flow (wrapping to the next line).

**Stack**
Items arranged along one axis. **VStack** = vertical (top to bottom).
**HStack** = horizontal (left to right). ptui uses `make-stack-widget`.

**Constraint**
A rule that limits how a widget sizes itself. Examples: max-width of 80
columns, fixed height of 5 rows, "fill remaining space." ptui has a
constraint solver (`ptui/constraints`).

**Inset / Padding**
Space between a widget's border and its content. `(insets top right bottom
left)`. An inset of 2 on each side means content renders 2 cells in from
the edge.

**Margin**
Space outside a widget's border, separating it from siblings. Not the same
as inset/padding — margin is external, padding is internal.

**Gutter**
A narrow column of space at the left or right edge of a content area.
In amoebum, the 1-character left gutter before message text. Also used
for line numbers, scroll indicators, or the `│` code block rule.

---

## Content Concepts

**Cell**
The atomic unit of terminal display. One cell = one character position.
Has: glyph (character), foreground color, background color, attributes
(bold, italic, dim, etc.). Wide characters (CJK) occupy 2 cells.

**Glyph**
The visible character in a cell. Can be a single codepoint or a grapheme
cluster (base + combining marks). Empty string = continuation cell (second
half of a wide character).

**Segment**
A run of text sharing the same style (role, bold, italic, etc.). A styled
line is a list of segments. `(:text "hello" :role :assistant :boldp t)`.

**Line**
One horizontal row of content. A logical line may wrap into multiple
display lines.

**Wrap / Soft wrap**
When a logical line exceeds the available width, it continues on the next
display row. The content is not truncated — it flows. Controlled by the
text layout engine.

**Truncation**
When a logical line exceeds the available width, the excess is cut off,
usually with an ellipsis. Opposite of wrapping.

**Scroll / Scrollback**
Moving the viewport over the canvas. Scrollback = how many rows above
the current viewport bottom you have scrolled. Scroll-follow = auto-scroll
to the bottom when new content arrives.

---

## Widget Concepts

**Widget**
A reusable UI component. Has props (input data), renders to elements,
can be memoized. Defined with `defwidget`.

**Element**
The output of a widget render. A node in the UI tree. Has a type, id,
props, and children. `(make-element :text :id :foo :props (:text "hi"))`.

**Tree**
The full hierarchy of elements that describes one frame of the UI.
Built top-down, rendered bottom-up. Reconciled against the previous
tree to compute minimal diffs.

**Box**
A widget that wraps children with optional border, padding, and
background. The basic container.

**Overlay**
A widget drawn on top of other content, occupying the same viewport
space. Approval dialogs and fuzzy pickers are overlays in amoebum.

---

## Height/Width Constraint Patterns

**Full height**
Widget expands to fill all available rows. Message history does this.

**Fixed height**
Widget occupies exactly N rows regardless of content. Status bar = 1 row.

**Content height**
Widget is as tall as its content requires. A short message = 1 row.

**Capped height (viewport cap)**
Widget has a maximum height. If content exceeds it, the excess is hidden
behind a scroll or collapse affordance. Shows "N more lines" or similar.
This is how amoebum renders thinking blocks — capped at ~5 rows,
expandable on demand.

**Min height**
Widget is at least N rows, even if content is shorter. Used to prevent
layout jumps.

**Fill / Flex**
Widget takes remaining space after fixed/content-sized siblings are laid
out. In a VStack of [fixed-1-row, fill, fixed-1-row], the fill widget
gets viewport-height minus 2.

---

## Styling Concepts

**Role**
A semantic label for content purpose. `:assistant`, `:user`, `:tool`,
`:meta`, `:warning`, `:error`, `:assistant-code`, `:user-label`, etc.
Roles map to visual styles through the theme.

**Theme**
A named collection of role-to-style mappings, colors, and markdown
config. Themes inherit from parent themes. The active theme controls
all visual output.

**Role style**
The concrete visual properties for a role: foreground color, background
color, bold, italic, dim, underline, inverse, strikethrough.

**Attributes (attrs)**
The non-color properties of a cell: boldp, italicp, underlinep, invertp,
dimp, strikep. Combined with fg/bg colors to form the complete cell style.

**Inverse / Reverse video**
Swaps foreground and background colors. Used for selection highlighting
or focus indicators. The old inline code rendering used this (now replaced
with a dedicated role).

**Dim**
Renders text at reduced brightness. Used for secondary/de-emphasized
content like the code block left rule `│`.

---

## Interaction Concepts

**Focus**
Which panel or widget receives keyboard input. In amoebum: prompt has
focus normally, approval dialog captures focus when active, fuzzy picker
captures focus when open.

**Mode**
A distinct interaction state. Normal mode (typing in prompt), scroll mode
(navigating history), plan mode (read-only), approval mode (yes/no dialog).

**Keybinding**
A mapping from a key (or key chord like Ctrl-O) to an action. Defined
with `defkeys`.

**Event**
A discrete input occurrence: key press, terminal resize, timer tick.
Events flow through the event bus and are dispatched to handlers.

---

## Rendering Pipeline

```
User input (key event)
  -> Event handler (updates state)
    -> Build UI tree (widgets render to elements)
      -> Layout solver (assigns sizes and positions)
        -> Cell buffer (elements -> grid of cells)
          -> Diff engine (compare to previous buffer)
            -> ANSI backend (emit only changed cells to terminal)
```

Each frame follows this pipeline. The diff engine ensures only changed
cells are redrawn, making updates efficient even for large UIs.

---

## Amoebum-Specific Terms

**Message history**
The scrollable list of conversation messages. Each message has a role
prefix (the colored `>`) and body text.

**Stream / Streaming**
LLM responses arrive token-by-token. The streaming renderer incrementally
parses markdown and updates the display as tokens arrive.

**Approval dialog**
The overlay that appears when a tool call needs user permission. Shows
tool name, details, and approve/deny options.

**Status bar**
The 1-row panel at the bottom showing context usage, permission mode,
streaming status, and plan mode indicators.

**Prompt box**
The text input area where the user types messages. Supports multiline
editing, Ctrl-U (clear), Ctrl-W (delete word), history search.
