# Add or update text in a Google Slides presentation

Add or update text in a Google Slides presentation

## Usage

``` r
add_text_multi(
  slide_obj,
  text,
  position,
  position_base = NULL,
  element_id = NULL,
  text_style = NULL,
  order = c("front", "back"),
  pass_strategy = c("one", "all"),
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

  A vector of character strings to add.

- position:

  A vector of objects of class
  [`r2slides::slide_position`](https://adam-hubbs.github.io/r2slides/reference/slide_position.md)

- position_base:

  A vector of objects of class
  [`r2slides::slide_position`](https://adam-hubbs.github.io/r2slides/reference/slide_position.md)

- element_id:

  Optional. A vector of string IDs of an existing text element to
  update. If element_id is `NULL` a new element will be created.

- text_style:

  Optional. A vector of text_style or style_rule objects.

- order:

  Optional. One of `"front"` or `"back"`. Controls the Z-order of each
  created element. Default: `"front"`. Ignored for elements updated via
  `element_id`.

- pass_strategy:

  Optional. A strategy to pass additional values to style_rule objects.

- debug:

  Optional. A logical indicating whether to print debug messages.
  Default: FALSE.

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
