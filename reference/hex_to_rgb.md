# Convert a hex color code to RGB values

Convert a hex color code to RGB values

## Usage

``` r
hex_to_rgb(hex_color)
```

## Arguments

- hex_color:

  A single string representing a 6-digit hex color code, with or without
  a leading "#".

## Value

A list with three components: \`red\`, \`green\`, and \`blue\`, each
containing a number between 0 and 1. Will error if the input is not a
valid 6-digit hex color code.
