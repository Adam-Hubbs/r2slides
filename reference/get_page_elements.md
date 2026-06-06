# Get page elements for a slide

Returns a tibble of all page elements on a slide, with columns
`element_id`, `type`, `left`, `top`, `width`, and `height` (dimensions
in inches). Useful for inspecting slide content and writing custom match
functions.

## Usage

``` r
get_page_elements(slide, ps = slide@presentation)
```

## Arguments

- slide:

  A
  [`r2slides::slide`](https://adam-hubbs.github.io/r2slides/reference/slide.md)
  object.

- ps:

  The presentation object owning the slide. Defaults to
  `slide@presentation`.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with columns `element_id`, `type`, `left`, `top`, `width`, `height`.

## Details

`type` is one of `"TEXT_BOX"`, `"SHEETS_CHART"`, `"IMAGE"`, `"TABLE"`,
or `"UNSUPPORTED"`.
