# Add a linked chart form Google Sheets

`add_linked_chart()` takes an existing Google Sheets chart and embeds it
in a Google Slides presentation. By default, it is *linked*, so updates
made to the chart will also update on the presentation.

## Usage

``` r
add_linked_chart(
  chart_obj,
  slide_obj,
  position,
  linked = TRUE,
  order = c("front", "back"),
  replacement_strategy = get_replacement_strategy(),
  match_fn = get_match_fn()
)
```

## Arguments

- chart_obj:

  A Google Sheets chart object

- slide_obj:

  A Google Slides slide object

- position:

  An object of class
  [`r2slides::slide_position`](https://adam-hubbs.github.io/r2slides/reference/slide_position.md)

- linked:

  Optional. A logical indicating whether the chart should be linked.

  - `TRUE` (the default): The chart is linked so updates made in the
    Google Sheet will also appear on the presentation.

  - `FALSE`: Inserts an image of the chart as it currently exists. Will
    not automatically update.

- order:

  Optional. One of `"front"` or `"back"`. Controls the Z-order of the
  created chart element. Default: `"front"`.

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

The Google Slides slide object

## Details

`add_linked_chart()` puts the inputed chart in the exact area of the
supplied `slides_position` object. This means that your chart may be
re-sized.

## Examples

``` r
if(FALSE) {
# Assumes that there is a chart already made on the references sheet
 chart_data |>
   write_gs("Seahawks vs Dolphins Points Per Game") |>
   get_chart_id() |>
   add_linked_chart(
     on_slide_number(
       4
     ),
     in_top_left()
  )
}
```
