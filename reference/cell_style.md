# Cell styling properties for table cells

Cell styling properties for table cells

## Usage

``` r
cell_style(
  bg_color = NULL,
  text_style = NULL,
  h_align = NULL,
  v_align = NULL,
  col_span = NULL,
  row_span = NULL
)
```

## Arguments

- bg_color:

  Background color. Accepts a hex string (e.g. \`"#FF0000"\`), a named R
  color (e.g. \`"red"\`), an RGB numeric vector \`c(r, g, b)\` with
  values in \\0, 1\\, or a theme color string (e.g. \`"ACCENT1"\`).

- text_style:

  A \`text_style\` object controlling the appearance of text within the
  cell.

- h_align:

  A string specifying horizontal alignment. One of
  \`"HORIZONTAL_ALIGNMENT_UNSPECIFIED"\`, \`"LEFT"\`, \`"CENTER"\`, or
  \`"RIGHT"\`.

- v_align:

  A string specifying vertical alignment. One of
  \`"VERTICAL_ALIGNMENT_UNSPECIFIED"\`, \`"TOP"\`, \`"MIDDLE"\`, or
  \`"BOTTOM"\`.

- col_span:

  An integer specifying the number of columns the cell spans.

- row_span:

  An integer specifying the number of rows the cell spans.
