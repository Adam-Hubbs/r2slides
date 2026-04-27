# Convert a table object to an r2slides_table

\`as_r2slides_table()\` is a generic function that converts a table
object (e.g. from \*\*gt\*\* or \*\*flextable\*\*) into an
\[r2slides_table\] object suitable for rendering as a Google Slides
table element.

Methods for specific table classes should return an \[r2slides_table\]
object.

## Usage

``` r
as_r2slides_table(x, ...)
```

## Arguments

- x:

  A table object. Methods are expected for classes such as \`gt_tbl\`
  (from \*\*gt\*\*) and \`flextable\` (from \*\*flextable\*\*).

- ...:

  Additional arguments passed to methods.

## Value

An \[r2slides_table\] object.
