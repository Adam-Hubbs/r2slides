# Add a table to a Google Slides slide

Converts \`table\` to an \[r2slides_table\] (if it isn't one already),
builds the necessary batchUpdate requests, and submits them in order to
the Slides API.

## Usage

``` r
add_table(
  slide_obj,
  table,
  position,
  order = c("front", "back"),
  table_id = NULL,
  debug = FALSE,
  token = NULL
)
```

## Arguments

- slide_obj:

  A \`r2slides::slide\` object.

- table:

  An \[r2slides_table\], or any object for which an
  \[as_r2slides_table()\] method exists (e.g. a \*\*flextable\*\*).

- position:

  A \[slide_position\] describing where to place the table.

- order:

  One of \`"front"\` or \`"back"\`. Controls the Z-order of the created
  element. Default: \`"front"\`.

- table_id:

  Optional character string. Object ID to assign to the new table.
  Auto-generated when \`NULL\`.

- debug:

  Logical. When \`TRUE\` the requests are returned instead of submitted
  to the API. Default: \`FALSE\`.

- token:

  Optional OAuth2 token. Defaults to \`r2slides_token()\`.

## Value

The \`slide_obj\` (invisibly).
