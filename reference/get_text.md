# Get the text content of an element

Concatenates the content of every text run in the element's text body
into a single character string. Paragraph markers and other non-run
entries are skipped. Only defined for `text_element` objects; calling on
any other element type errors with an informative message.

## Usage

``` r
get_text(x, ...)
```

## Arguments

- x:

  A `text_element` object.

## Value

A single character string containing the full text of the element. An
empty string (`""`) is returned if the element has no text runs.

## See also

[`get_text_style()`](https://adam-hubbs.github.io/r2slides/reference/get_text_style.md),
[`get_raw()`](https://adam-hubbs.github.io/r2slides/reference/get_raw.md)

## Examples

``` r
if (FALSE) {
  ps <- register_presentation(id = "YOUR_PRESENTATION_ID", set_active = FALSE)
  slide_obj <- on_slide_number(1, ps)
  el <- get_elements(slide_obj, type = "text")[[1]]
  get_text(el)
}
```
