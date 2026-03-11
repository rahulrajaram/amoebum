#!/usr/bin/env python3
"""ansi-to-png.py — Render an ANSI-escaped text file to a PNG image.

Uses pyte (terminal emulator) to parse escape sequences and Pillow to render
the result as a monospace-font image, simulating how the terminal looks.

Usage:
    python3 ansi-to-png.py input.ansi output.png [--cols 120] [--rows 40]

Falls back to ImageMagick's `convert` if pyte/Pillow are unavailable.
"""

import sys
import os
import subprocess
import shutil


def _resolve_color(color_val, fallback):
    """Resolve a pyte color value to an RGB tuple.

    pyte returns:
    - "default" for default fg/bg
    - Named colors: "red", "green", etc.
    - Hex RGB strings: "82d2ff" (from 38;2;R;G;B sequences)
    """
    if color_val == "default" or not color_val:
        return fallback

    # Named color lookup
    NAMED = {
        "black": (0, 0, 0),
        "red": (205, 49, 49),
        "green": (13, 188, 121),
        "yellow": (229, 229, 16),
        "blue": (36, 114, 200),
        "magenta": (188, 63, 188),
        "cyan": (17, 168, 205),
        "white": (204, 204, 204),
        # Bright variants
        "brightblack": (102, 102, 102),
        "brightred": (241, 76, 76),
        "brightgreen": (35, 209, 139),
        "brightyellow": (245, 245, 67),
        "brightblue": (59, 142, 234),
        "brightmagenta": (214, 112, 214),
        "brightcyan": (41, 184, 219),
        "brightwhite": (242, 242, 242),
    }
    if color_val in NAMED:
        return NAMED[color_val]

    # Hex RGB string from pyte (e.g. "82d2ff")
    if len(color_val) == 6:
        try:
            return (int(color_val[0:2], 16),
                    int(color_val[2:4], 16),
                    int(color_val[4:6], 16))
        except ValueError:
            pass

    return fallback


def _load_fonts(font_size=14):
    """Load regular, bold, and italic monospace fonts. Returns (fonts_dict, cell_w, cell_h)."""
    from PIL import ImageFont
    import math

    font_families = [
        {
            "regular": "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf",
            "bold": "/usr/share/fonts/truetype/dejavu/DejaVuSansMono-Bold.ttf",
            "italic": "/usr/share/fonts/truetype/dejavu/DejaVuSansMono-Oblique.ttf",
            "bold_italic": "/usr/share/fonts/truetype/dejavu/DejaVuSansMono-BoldOblique.ttf",
        },
        {
            "regular": "/usr/share/fonts/truetype/liberation/LiberationMono-Regular.ttf",
            "bold": "/usr/share/fonts/truetype/liberation/LiberationMono-Bold.ttf",
            "italic": "/usr/share/fonts/truetype/liberation/LiberationMono-Italic.ttf",
            "bold_italic": "/usr/share/fonts/truetype/liberation/LiberationMono-BoldItalic.ttf",
        },
    ]

    for family in font_families:
        if os.path.exists(family["regular"]):
            fonts = {}
            for variant, path in family.items():
                if os.path.exists(path):
                    fonts[variant] = ImageFont.truetype(path, font_size)
                else:
                    fonts[variant] = None
            # Fill missing variants with regular
            for v in ("bold", "italic", "bold_italic"):
                if fonts[v] is None:
                    fonts[v] = fonts["regular"]

            # Use getlength() for accurate advance width (not getbbox which is glyph bounds)
            cell_w = math.ceil(fonts["regular"].getlength("M"))
            # Line height: ascent + descent with padding
            ascent, descent = fonts["regular"].getmetrics()
            cell_h = ascent + descent + 2
            return fonts, cell_w, cell_h

    # Fallback to default font
    default = ImageFont.load_default()
    return {"regular": default, "bold": default, "italic": default, "bold_italic": default}, 8, 16


def render_with_pyte_pillow(input_path, output_path, cols=120, rows=40):
    """Render ANSI file to PNG using pyte + Pillow."""
    import pyte
    from PIL import Image, ImageDraw

    with open(input_path, "r", errors="replace") as f:
        content = f.read()

    # tmux capture-pane outputs bare LF between rows. pyte treats LF as
    # "move down" without resetting the column, so we must add CR before
    # each LF to get proper column-0 alignment per row.
    content = content.replace("\n", "\r\n").rstrip("\r\n")

    # Parse through pyte terminal emulator
    screen = pyte.Screen(cols, rows)
    stream = pyte.Stream(screen)
    stream.feed(content)

    DEFAULT_FG = (204, 204, 204)
    DEFAULT_BG = (30, 30, 30)

    fonts, cell_w, cell_h = _load_fonts(font_size=14)

    # Padding
    pad = 8
    img_w = cols * cell_w + pad * 2
    img_h = rows * cell_h + pad * 2
    img = Image.new("RGB", (img_w, img_h), DEFAULT_BG)
    draw = ImageDraw.Draw(img)

    for y in range(rows):
        for x in range(cols):
            char = screen.buffer[y][x]

            fg = _resolve_color(char.fg, DEFAULT_FG)
            bg = _resolve_color(char.bg, DEFAULT_BG)

            # Handle reverse video
            if char.reverse:
                fg, bg = bg, fg

            # Bold brightens default fg
            if char.bold and fg == DEFAULT_FG:
                fg = (242, 242, 242)

            # Draw background cell if non-default
            cell_x = pad + x * cell_w
            cell_y = pad + y * cell_h
            if bg != DEFAULT_BG:
                draw.rectangle(
                    [cell_x, cell_y, cell_x + cell_w, cell_y + cell_h],
                    fill=bg,
                )

            ch = char.data
            if ch and ch != " ":
                # Select font variant
                if char.bold and char.italics:
                    font = fonts["bold_italic"]
                elif char.bold:
                    font = fonts["bold"]
                elif char.italics:
                    font = fonts["italic"]
                else:
                    font = fonts["regular"]

                # Center glyph in cell for box-drawing alignment
                glyph_w = font.getlength(ch)
                x_offset = (cell_w - glyph_w) / 2
                draw.text((cell_x + x_offset, cell_y), ch, fill=fg, font=font)

    img.save(output_path)


def render_with_imagemagick(input_path, output_path, cols=120, rows=40):
    """Fallback: strip ANSI codes and render plain text to PNG via ImageMagick."""
    import re

    with open(input_path, "r", errors="replace") as f:
        content = f.read()

    # Strip ANSI escape sequences
    clean = re.sub(r"\x1b\[[0-9;]*[a-zA-Z]", "", content)

    # Write temp clean file
    tmp = input_path + ".clean.txt"
    with open(tmp, "w") as f:
        f.write(clean)

    try:
        subprocess.run(
            [
                "convert",
                "-size", f"{cols * 8 + 20}x{rows * 16 + 20}",
                "xc:#1e1e1e",
                "-font", "DejaVu-Sans-Mono",
                "-pointsize", "14",
                "-fill", "#cccccc",
                "-annotate", "+10+20",
                f"@{tmp}",
                output_path,
            ],
            check=True,
            capture_output=True,
        )
    finally:
        os.unlink(tmp)


def main():
    if len(sys.argv) < 3:
        print(f"Usage: {sys.argv[0]} input.ansi output.png [--cols N] [--rows N]")
        sys.exit(1)

    input_path = sys.argv[1]
    output_path = sys.argv[2]
    cols = 120
    rows = 40

    for i, arg in enumerate(sys.argv[3:], start=3):
        if arg == "--cols" and i + 1 < len(sys.argv):
            cols = int(sys.argv[i + 1])
        elif arg == "--rows" and i + 1 < len(sys.argv):
            rows = int(sys.argv[i + 1])

    try:
        import pyte  # noqa: F401  -- test availability
        from PIL import ImageFont  # noqa: F401  -- test availability
        del pyte, ImageFont
        render_with_pyte_pillow(input_path, output_path, cols, rows)
    except ImportError as e:
        missing = str(e)
        if shutil.which("convert"):
            print(f"  (pyte/Pillow unavailable [{missing}], falling back to ImageMagick)")
            render_with_imagemagick(input_path, output_path, cols, rows)
        else:
            print(f"  WARNING: Cannot render PNG — install pyte+Pillow or ImageMagick")
            sys.exit(0)  # non-fatal


if __name__ == "__main__":
    main()
