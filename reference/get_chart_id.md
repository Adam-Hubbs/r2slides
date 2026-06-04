# Get chart_id from a Google Sheet

Retrieves the chart ID from a specified sheet. Errors if 0 or more than
1 chart is found.

## Usage

``` r
get_chart_id(spreadsheet_obj, token = NULL, call = rlang::caller_env())
```

## Arguments

- spreadsheet_obj:

  A `sht_id` object.

- token:

  Optional. An OAuth2 token. The default uses
  [`r2slides_token()`](https://adam-hubbs.github.io/r2slides/reference/r2slides_token.md).

- call:

  Optional. Call environment used in error messages.

## Value

A `chart_id` object
