# Get the type of an element

Returns a single lowercase string identifying the concrete type of an
element object. Useful for dispatching on type or for filtering the
output of
[`get_elements()`](https://adam-hubbs.github.io/r2slides/reference/get_elements.md).

## Usage

``` r
element_type(x, ...)
```

## Arguments

- x:

  An `element` object or subclass.

## Value

A single character string: one of `"text"`, `"image"`, `"chart"`,
`"table"`, `"shape"`, or `"unknown"` (the last for the base `element`
class when no known key is present in the raw API node).

## See also

[element](https://adam-hubbs.github.io/r2slides/reference/element.md),
[`get_elements()`](https://adam-hubbs.github.io/r2slides/reference/get_elements.md)

## Examples

``` r
if (FALSE) {
  ps <- register_presentation(id = "YOUR_PRESENTATION_ID", set_active = FALSE)
  slide_obj <- on_slide_number(1, ps)
  els <- get_all_elements(slide_obj)

  purrr::map_chr(els, element_type)
}
```
