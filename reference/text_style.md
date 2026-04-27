# Text styling properties

Text styling properties

## Usage

``` r
text_style(
  bg_color = NULL,
  text_color = NULL,
  bold = NULL,
  italic = NULL,
  font_family = NULL,
  font_size = NULL,
  link = NULL,
  baseline_offset = NULL,
  small_caps = NULL,
  strikethrough = NULL,
  underline = NULL,
  alignment = NULL,
  line_spacing = NULL,
  indent_start = NULL,
  indent_end = NULL,
  space_above = NULL,
  space_below = NULL,
  indent_first_line = NULL,
  direction = NULL,
  spacing_mode = NULL
)
```

## Arguments

- bg_color:

  Background color. Accepts a hex string (e.g. \`"#FF0000"\`), a named R
  color (e.g. \`"red"\`), an RGB numeric vector \`c(r, g, b)\` with
  values in \\0, 1\\, or a theme color string (e.g. \`"ACCENT1"\`).

- text_color:

  Text (foreground) color. Same formats as \`bg_color\`.

- bold:

  A logical indicating whether the text should be bold.

- italic:

  A logical indicating whether the text should be italic.

- font_family:

  A string specifying the font family.

- font_size:

  A numeric specifying the font size in points.

- link:

  A string specifying the link URL.

- baseline_offset:

  A numeric specifying the baseline offset in points.

- small_caps:

  A logical indicating whether the text should be in small caps.

- strikethrough:

  A logical indicating whether the text should be strikethrough.

- underline:

  A logical indicating whether the text should be underlined.

- alignment:

  Paragraph horizontal alignment. One of \`"ALIGNMENT_UNSPECIFIED"\`,
  \`"START"\`, \`"CENTER"\`, \`"END"\`, or \`"JUSTIFIED"\`.

- line_spacing:

  Line spacing as a percentage of normal, e.g. \`100\` for
  single-spacing, \`200\` for double-spacing. Must be \>= 0.

- indent_start:

  Indent from the start side (left for LTR), in points.

- indent_end:

  Indent from the end side (right for LTR), in points.

- space_above:

  Extra space above the paragraph, in points.

- space_below:

  Extra space below the paragraph, in points.

- indent_first_line:

  First-line indent, in points.

- direction:

  Text direction. One of \`"TEXT_DIRECTION_UNSPECIFIED"\`,
  \`"LEFT_TO_RIGHT"\`, or \`"RIGHT_TO_LEFT"\`.

- spacing_mode:

  Spacing mode. One of \`"SPACING_MODE_UNSPECIFIED"\`,
  \`"NEVER_COLLAPSE"\`, or \`"COLLAPSE_LISTS"\`.
