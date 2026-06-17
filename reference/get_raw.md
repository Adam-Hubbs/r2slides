# Get the raw API node for an element

Fetches the raw page-element list from the Google Slides API and returns
the entry whose `objectId` matches `x@element_id`. This is the single
point of API contact for all element readers; all other getters
([`get_position()`](https://adam-hubbs.github.io/r2slides/reference/get_position.md),
[`get_text()`](https://adam-hubbs.github.io/r2slides/reference/get_text.md),
etc.) call `get_raw()` first and then parse the result locally.

The slide's page data is cached with a 15-second TTL by
`presentation$get_page_raw()`, so repeated calls within the same session
do not necessarily trigger an HTTP request.

## Usage

``` r
get_raw(x, ...)
```

## Arguments

- x:

  An `element` object or subclass.

## Value

A named list representing one entry from the Google Slides API
`pageElements` array.

## See also

[`get_position()`](https://adam-hubbs.github.io/r2slides/reference/get_position.md),
[`get_text()`](https://adam-hubbs.github.io/r2slides/reference/get_text.md),
[`get_text_style()`](https://adam-hubbs.github.io/r2slides/reference/get_text_style.md),
[`get_image()`](https://adam-hubbs.github.io/r2slides/reference/get_image.md),
[`get_linked_sheet()`](https://adam-hubbs.github.io/r2slides/reference/get_linked_sheet.md),
[`get_table()`](https://adam-hubbs.github.io/r2slides/reference/get_table.md)

## Examples

``` r
if (FALSE) {
  ps <- register_presentation(id = "YOUR_PRESENTATION_ID", set_active = FALSE)
  slide_obj <- on_slide_number(1, ps)
  el <- get_elements(slide_obj, type = "text")[[1]]
  raw <- get_raw(el)
  names(raw)
}
```
