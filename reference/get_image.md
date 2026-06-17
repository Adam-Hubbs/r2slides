# Get image URL or file path from an image element

Extracts the content URL for an image element from the raw API node. By
default the URL is returned as a string. Set `download = TRUE` to
download the image to a temporary file and return the file path instead.

The content URL is a short-lived signed URL issued by Google; it is
valid for the duration of the current session but should not be stored
permanently.

Only defined for `image_element` objects; calling on any other element
type errors with an informative message.

## Usage

``` r
get_image(x, ...)
```

## Arguments

- x:

  An `image_element` object.

- download:

  Logical. If `FALSE` (default), returns the content URL as a character
  string. If `TRUE`, downloads the image to a temp file (`.png`
  extension) and returns the file path.

## Value

A single character string: either the signed content URL
(`download = FALSE`) or an absolute path to a temporary file
(`download = TRUE`).

## See also

[`get_raw()`](https://adam-hubbs.github.io/r2slides/reference/get_raw.md),
[`add_image()`](https://adam-hubbs.github.io/r2slides/reference/add_image.md)

## Examples

``` r
if (FALSE) {
  ps <- register_presentation(id = "YOUR_PRESENTATION_ID", set_active = FALSE)
  slide_obj <- on_slide_number(1, ps)
  el <- get_elements(slide_obj, type = "image")[[1]]

  # Get the signed URL
  url <- get_image(el)

  # Download to a temp file
  path <- get_image(el, download = TRUE)
}
```
