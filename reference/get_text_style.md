# Get the text style of an element

Inspects every text run in the element and returns a styling object that
can be passed directly to
[`add_text()`](https://adam-hubbs.github.io/r2slides/reference/add_text.md)
via its `text_style` argument:

- **Uniform style** (all runs share identical character-level styles):
  returns a single `text_style` object.

- **Mixed styles** (runs differ in at least one style property): returns
  a `style_rule` with one `func`-type selector per run. Each selector
  function returns `c(start, end)` 1-based inclusive character indices
  for that run, matching the convention expected by
  [`create_styling_request()`](https://adam-hubbs.github.io/r2slides/reference/create_styling_request.md).

Note: only character-level styles (bold, italic, font size, color, etc.)
are captured. Paragraph-level styles (alignment, line spacing,
indentation) live on `paragraphMarker` entries, which `text_runs()`
skips, so they will not be reflected in the returned object.

## Usage

``` r
get_text_style(x, ...)
```

## Arguments

- x:

  A `text_element` object.

## Value

A `text_style` object (uniform case) or a `style_rule` object
(mixed-style case). Either can be passed directly to
`add_text(text_style = ...)` to reproduce the styling on a new element.

## See also

[`get_text()`](https://adam-hubbs.github.io/r2slides/reference/get_text.md),
[`text_style()`](https://adam-hubbs.github.io/r2slides/reference/text_style.md),
[`style_rule()`](https://adam-hubbs.github.io/r2slides/reference/style_rule.md)

## Examples

``` r
if (FALSE) {
  ps <- register_presentation(id = "YOUR_PRESENTATION_ID", set_active = FALSE)
  slide_obj <- on_slide_number(1, ps)
  el <- get_elements(slide_obj, type = "text")[[1]]

  sty <- get_text_style(el)
  # sty is a text_style for uniform styling or a style_rule for mixed styling

  # Recreate the element at the same position with the extracted style:
  add_text(
    slide_obj,
    get_text(el),
    get_position(el),
    text_style = sty
  )
}
```
