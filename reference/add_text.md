# Add text in a Google Slides presentation

Adds text into a new text box on a Google Slides presentation. Text can
be formatted by text_style.

## Usage

``` r
add_text(
  slide_obj,
  text,
  position,
  element_id = NULL,
  text_style = NULL,
  order = c("front", "back"),
  debug = FALSE,
  replacement_strategy = get_replacement_strategy(),
  match_fn = get_match_fn(),
  ...
)
```

## Arguments

- slide_obj:

  A Google Slides slide object.

- text:

  A character string of text to add.

- position:

  An object of class
  [`r2slides::slide_position`](https://adam-hubbs.github.io/r2slides/reference/slide_position.md)

- element_id:

  Optional. A string ID of an existing text element to update. If
  element_id is `NULL` a new element will be created.

- text_style:

  Optional. A list of text styling properties. One of:

  - NULL (the default): Styling is determined by the defaults for the
    Google Slides presentation

  - r2slides::text_style object: List of Text styling

  - r2slides::style_rule object: Conditionally formatted object that
    defined styles and when to use those styles. Resolves to a
    text_style object.

- order:

  Optional. One of `"front"` or `"back"`. Controls the Z-order of the
  created element. Default: `"front"`. Ignored when updating an existing
  element via `element_id`.

- debug:

  Optional. A logical indicating whether to return the request objects,
  or evaluate them. Default: FALSE.

- replacement_strategy:

  One of `"add"` (default), `"replace"`, or `"skip"`. Overrides the
  package-wide default set by
  [`set_replacement_strategy()`](https://adam-hubbs.github.io/r2slides/reference/set_replacement_strategy.md).
  `"add"` always creates a new element. `"replace"` deletes any matching
  element and creates a fresh one. `"skip"` leaves a matching element
  untouched and suppresses creation.

- match_fn:

  A function that identifies an existing slide element matching the new
  one. Must have signature
  `function(new_spec, existing_elements) -> character(1) | NULL`. See
  [`match_by_type_and_position()`](https://adam-hubbs.github.io/r2slides/reference/match_by_type_and_position.md)
  for details and the built-in implementation. Overrides the
  package-wide default set by
  [`set_match_fn()`](https://adam-hubbs.github.io/r2slides/reference/set_match_fn.md).

- ...:

  Additional values available to style_rule objects.

## Value

The Google Slides slide object (invisibly).

## Examples

``` r
if(FALSE) {
on_slide_number(2) |>
  add_text("Hello there!", position = in_top_left())
}
```
