# Get the position of an element

Parses the element's affine transform from the raw API node and returns
a `slide_position` object encoding top, left, width, height (all in
inches), and rotation (in degrees). Works on any element subclass.

Google Slides stores positions using an affine transform matrix
(`scaleX`, `scaleY`, `shearX`, `shearY`, `translateX`, `translateY`)
plus a nominal size. `get_position()` decomposes the matrix to recover
the rendered dimensions and rotation angle.

## Usage

``` r
get_position(x, ...)
```

## Arguments

- x:

  An `element` object or subclass.

## Value

A `slide_position` object. Can be passed directly to
[`add_text()`](https://adam-hubbs.github.io/r2slides/reference/add_text.md),
[`add_image()`](https://adam-hubbs.github.io/r2slides/reference/add_image.md),
etc. to recreate the element at the same location.

## See also

[`slide_position()`](https://adam-hubbs.github.io/r2slides/reference/slide_position.md),
[`get_raw()`](https://adam-hubbs.github.io/r2slides/reference/get_raw.md)

## Examples

``` r
if (FALSE) {
  ps <- register_presentation(id = "YOUR_PRESENTATION_ID", set_active = FALSE)
  slide_obj <- on_slide_number(1, ps)
  el <- get_elements(slide_obj, type = "text")[[1]]
  pos <- get_position(el)
  pos@top
  pos@left
}
```
