# Set default style values

This function provides a way to set defaults for all sorts of uses
within the themes we established at y2. One can create a new style
easily and in one place, and it will update the defaults across all
r2slides functions.

## Usage

``` r
set_defaults(
  args,
  type = c("title", "commentary", "footer"),
  report_style = c("qualtrics", "municipal", "y2")
)
```

## Arguments

- args:

  A named list of style arguments. If you provide a value, it will not
  be overrided.

- type:

  One of \`"title"\`, \`"commentary"\`, or \`"footer"\`.

- report_style:

  One of \`"qualtrics"\`, \`"municipal"\`, or \`"y2"\`.

## Value

A list of style arguments with defaults applied based on the \`type\`
and \`report_style\`. User supplied values will propogate.
