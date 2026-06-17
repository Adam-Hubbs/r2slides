# Get table cell text from a table element

Extracts the text content from each cell of a Google Slides table
element and returns it as a tidy tibble with one row per cell. If a cell
contains multiple text runs they are concatenated before being returned.

Only defined for `table_element` objects; calling on any other element
type errors with an informative message.

## Usage

``` r
get_table(x, ...)
```

## Arguments

- x:

  A `table_element` object.

## Value

A tibble with columns:

- `row` (`integer`): 1-based row index.

- `col` (`integer`): 1-based column index.

- `text` (`character`): concatenated text content of the cell.

## Details

The current implementation captures text only. Cell styles, borders, and
merged-cell spans are not yet extracted. Full `r2slides_table`
reconstruction is planned for a future release.

## See also

[`get_raw()`](https://adam-hubbs.github.io/r2slides/reference/get_raw.md),
[table_element](https://adam-hubbs.github.io/r2slides/reference/element.md)

## Examples

``` r
if (FALSE) {
  ps <- register_presentation(id = "YOUR_PRESENTATION_ID", set_active = FALSE)
  slide_obj <- on_slide_number(1, ps)
  el <- get_elements(slide_obj, type = "table")[[1]]
  tbl <- get_table(el)
  tbl
}
```
