# Get the linked Google Sheet chart reference from a chart element

Resolves the chart's `spreadsheetId` and `chartId` from the raw API node
back to a `chart_id` object. The function makes one API call to the
Google Sheets API to identify which sheet tab within the spreadsheet
contains the chart, so the returned `chart_id` carries `spreadsheet_id`,
`sheet_id`, and `chart_id`.

This is useful for round-tripping: extract the reference from an
existing linked chart element and pass it to
[`add_linked_chart()`](https://adam-hubbs.github.io/r2slides/reference/add_linked_chart.md)
to insert it on another slide.

Only defined for `chart_element` objects; calling on any other element
type errors with an informative message.

## Usage

``` r
get_linked_sheet(x, ...)
```

## Arguments

- x:

  A `chart_element` object.

## Value

A `chart_id` S7 object with properties `spreadsheet_id`, `sheet_id`, and
`chart_id`.

## See also

[`get_raw()`](https://adam-hubbs.github.io/r2slides/reference/get_raw.md),
[`chart_id()`](https://adam-hubbs.github.io/r2slides/reference/chart_id.md)

## Examples

``` r
if (FALSE) {
  ps <- register_presentation(id = "YOUR_PRESENTATION_ID", set_active = FALSE)
  slide_obj <- on_slide_number(1, ps)
  el <- get_elements(slide_obj, type = "chart")[[1]]
  cid <- get_linked_sheet(el)
  cid@spreadsheet_id
  cid@chart_id
}
```
