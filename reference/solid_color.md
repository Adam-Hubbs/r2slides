# Opaque color for use in text styles

Represents a Google Slides \`opaqueColor\` - a color with no alpha
channel. Used for text foreground and background colors
(\`updateTextStyle\`).

## Usage

``` r
solid_color(color = NULL)
```

## Arguments

- color:

  A color value in any accepted format (see above).

## Details

Accepts a hex string (\`"#RRGGBB"\`), a named R color (e.g. \`"red"\`),
an RGB numeric vector \`c(r, g, b)\` with values in \[0, 1\], or a
Google Slides theme color string (e.g. \`"ACCENT1"\`).
