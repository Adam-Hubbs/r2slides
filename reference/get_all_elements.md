# Get all elements in a slide

Returns every page element on the slide as a list of typed element
objects (`text_element`, `image_element`, etc.). No filtering is
applied; use
[`get_elements()`](https://adam-hubbs.github.io/r2slides/reference/get_elements.md)
if you want to filter by type or spatial region.

## Usage

``` r
get_all_elements(slide)
```

## Arguments

- slide:

  A `slide` object (e.g. from
  [`on_slide_id()`](https://adam-hubbs.github.io/r2slides/reference/on_slide_id.md)
  or
  [`on_slide_number()`](https://adam-hubbs.github.io/r2slides/reference/on_slide_id.md)).

## Value

A list of element objects. Each entry is one of `text_element`,
`image_element`, `chart_element`, `table_element`, `shape_element`, or
the base `element` class for unrecognised nodes. The list is empty
([`list()`](https://rdrr.io/r/base/list.html)) when the slide has no
elements.

## See also

[`get_elements()`](https://adam-hubbs.github.io/r2slides/reference/get_elements.md)
for filtered retrieval,
[`element_type()`](https://adam-hubbs.github.io/r2slides/reference/element_type.md)
to inspect individual elements.

## Examples

``` r
if (FALSE) {
  ps <- register_presentation(id = "YOUR_PRESENTATION_ID", set_active = FALSE)
  slide_obj <- on_slide_number(1, ps)
  els <- get_all_elements(slide_obj)
  length(els)
}
```
