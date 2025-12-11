# Testing function for how to use define_relative_transformation_function

Testing function for how to use define_relative_transformation_function

## Usage

``` r
chart_annotation_1(position)
```

## Arguments

- position:

  An object of class \`r2slides::slide_position\`

## Value

An object of class \`r2slides::slide_position\`

## Examples

``` r
if (FALSE) { # \dontrun{
data |>
  write_gs("Sheet Name") |>
  add_linked_chart(on_slide_url("URL Of Slide"), in_top_left()) |>
  add_text("Title", in_top_left() |> chart_annotation_1())
} # }
```
