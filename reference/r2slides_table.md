# Intermediate table representation for Google Slides

\`r2slides_table\` is a backend-agnostic intermediate representation of
a table, intended to be the target output of \[as_r2slides_table()\]
methods for table objects such as those produced by \*\*gt\*\* or
\*\*flextable\*\*. The object captures cell locations and styles,
dimensions, and border styling so that it can be rendered as a Google
Slides table element.

## Usage

``` r
r2slides_table(
  cells = list(),
  n_rows = integer(0),
  n_cols = integer(0),
  col_widths = NULL,
  row_heights = NULL,
  header_rows = 0L,
  border_styles = NULL
)
```

## Arguments

- cells:

  A list of \[table_cell\] objects defining the 0-based row/column start
  index of each cell along with its optional \[cell_style\].

- n_rows:

  A positive integer giving the total number of rows.

- n_cols:

  A positive integer giving the total number of columns.

- col_widths:

  A numeric vector of column widths in points. Must have length equal to
  \`n_cols\` when provided.

- row_heights:

  A numeric vector of row heights in points. Must have length equal to
  \`n_rows\` when provided.

- header_rows:

  A non-negative integer giving the number of header rows. Defaults to
  \`0L\`.
