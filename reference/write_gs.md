# Write data to a Google Sheet

`write_gs()` takes data and writes it to a Sheet. It is a wrapper around
[`googlesheets4::range_write()`](https://googlesheets4.tidyverse.org/reference/range_write.html).

## Usage

``` r
write_gs(data, sheet, spreadsheet = get_active_spreadsheet())
```

## Arguments

- data:

  A data frame to write to the Google Sheet.

- sheet:

  The name of the sheet to write to.

- spreadsheet:

  Optional. An
  [`r2slides::spreadsheet`](https://adam-hubbs.github.io/r2slides/reference/spreadsheet.md)
  object, a
  [googlesheets4::sheets_id](https://googlesheets4.tidyverse.org/reference/sheets_id.html),
  or any object accepted by
  [`googlesheets4::as_sheets_id()`](https://googlesheets4.tidyverse.org/reference/sheets_id.html)
  (spreadsheet ID string or URL). Defaults to
  [`get_active_spreadsheet()`](https://adam-hubbs.github.io/r2slides/reference/get_active_spreadsheet.md).

## Value

A `sht_id` object.

## Examples

``` r
if(FALSE){
  mtcars |>
    write_gs("Cars")
}
```
