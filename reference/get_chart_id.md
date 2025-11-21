# Get chart_id from a Google Sheet

Retrieves Sheet object from a specified sheet. Errors is 0 or more than
1 chart is found.

## Usage

``` r
get_chart_id(spreadsheet_obj, token = NULL, call = rlang::caller_env())
```

## Arguments

- spreadsheet_obj:

  A list of spreadsheet_id (string) and sheet_id (integer).

- token:

  Optional. An OAuth2 token. The default uses \`r2slides_token()\` to
  find a token.

- call:

  Optional. Call environment used in error messages.

## Value

A list of the spreadsheet_id (string) the sheet_id (integer) and
chart_id (integer)
