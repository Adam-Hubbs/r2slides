# Register an existing Google Sheets spreadsheet

Registers an existing Google Sheets spreadsheet so you can use it in
r2slides. Supply a spreadsheet ID string or a Google Sheets URL.

## Usage

``` r
register_spreadsheet(id, set_active = TRUE)
```

## Arguments

- id:

  A spreadsheet ID string or Google Sheets URL.

- set_active:

  Optional. A logical value indicating whether to set the spreadsheet as
  the active spreadsheet. Default: `TRUE`.

## Value

A `spreadsheet` object
