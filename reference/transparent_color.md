# Color with optional alpha for use in fills

Represents a Google Slides \`solidFill\` - a color with an optional
alpha (opacity) value. Used for table cell background fills and border
fills (\`updateTableCellProperties\`, \`updateTableBorderProperties\`).

## Usage

``` r
transparent_color(color = solid_color(), alpha = NULL)
```

## Arguments

- color:

  A color value in any format accepted by \[solid_color()\].

- alpha:

  \`NULL\` or a single number in \[0, 1\].

## Details

The \`color\` argument accepts the same formats as \[solid_color()\].
\`alpha\` is a number in \[0, 1\] where \`0\` is fully transparent and
\`1\` is fully opaque. When \`NULL\` (the default) the alpha field is
omitted from the API request, which the API treats as fully opaque.
