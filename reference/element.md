# Element objects

S7 classes representing individual content items on a Google Slides
slide. Each element holds three pieces of context — the parent
`presentation`, the parent `slide`, and the API `element_id` — that
allow any generic to fetch the element's raw data on demand without
storing a snapshot.

The concrete subclasses are:

- `text_element` — text boxes (shape type `TEXT_BOX`)

- `image_element` — inline images

- `chart_element` — linked Google Sheets charts

- `table_element` — tables

- `shape_element` — other shapes (rectangles, ellipses, etc.)

Use
[`element_type()`](https://adam-hubbs.github.io/r2slides/reference/element_type.md)
to identify the type at runtime, and
[`get_elements()`](https://adam-hubbs.github.io/r2slides/reference/get_elements.md)
or
[`get_all_elements()`](https://adam-hubbs.github.io/r2slides/reference/get_all_elements.md)
to obtain element objects from a slide.

## Usage

``` r
element(presentation = NULL, slide = NULL, element_id = character(0))

text_element(presentation = NULL, slide = NULL, element_id = character(0))

image_element(presentation = NULL, slide = NULL, element_id = character(0))

chart_element(presentation = NULL, slide = NULL, element_id = character(0))

table_element(presentation = NULL, slide = NULL, element_id = character(0))

shape_element(presentation = NULL, slide = NULL, element_id = character(0))
```

## Arguments

- presentation:

  A `presentation` R6 object (from
  [`register_presentation()`](https://adam-hubbs.github.io/r2slides/reference/register_presentation.md)
  or
  [`new_presentation()`](https://adam-hubbs.github.io/r2slides/reference/new_presentation.md)).

- slide:

  A `slide` S7 object (from
  [`on_slide_id()`](https://adam-hubbs.github.io/r2slides/reference/on_slide_id.md)
  or similar selectors).

- element_id:

  A single character string: the element's Google Slides object ID.

## Value

An S7 object of the corresponding element class.

## See also

[`get_elements()`](https://adam-hubbs.github.io/r2slides/reference/get_elements.md),
[`get_all_elements()`](https://adam-hubbs.github.io/r2slides/reference/get_all_elements.md),
[`element_type()`](https://adam-hubbs.github.io/r2slides/reference/element_type.md),
[`get_raw()`](https://adam-hubbs.github.io/r2slides/reference/get_raw.md),
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

  # Retrieve all elements on slide 1
  els <- get_all_elements(slide_obj)

  # Check the type of the first element
  element_type(els[[1]])

  # Filter to text elements only
  text_els <- get_elements(slide_obj, type = "text")
}
```
