# Add a table to a Google Slides slide

Converts `table` to an
[r2slides_table](https://adam-hubbs.github.io/r2slides/reference/r2slides_table.md)
(if it isn't one already), builds the necessary batchUpdate requests,
and submits them in order to the Slides API.

## Usage

``` r
add_table(
  slide_obj,
  table,
  position,
  order = c("front", "back"),
  table_id = NULL,
  replacement_strategy = get_replacement_strategy(),
  match_fn = get_match_fn()
)
```

## Arguments

- slide_obj:

  A
  [`r2slides::slide`](https://adam-hubbs.github.io/r2slides/reference/slide.md)
  object.

- table:

  An
  [r2slides_table](https://adam-hubbs.github.io/r2slides/reference/r2slides_table.md),
  or any object for which an
  [`as_r2slides_table()`](https://adam-hubbs.github.io/r2slides/reference/as_r2slides_table.md)
  method exists (e.g. a **flextable**).

- position:

  A
  [slide_position](https://adam-hubbs.github.io/r2slides/reference/slide_position.md)
  describing where to place the table.

- order:

  One of `"front"` or `"back"`. Controls the Z-order of the created
  element. Default: `"front"`.

- table_id:

  Optional character string. Object ID to assign to the new table.
  Auto-generated when `NULL`.

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

## Value

The `slide_obj` (invisibly).
