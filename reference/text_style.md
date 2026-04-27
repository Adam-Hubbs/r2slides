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
  underline = NULL
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
