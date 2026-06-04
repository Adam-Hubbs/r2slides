# Table Objects

`r2slides_table` is the internal representation of a table in r2slides.
It holds cell-level content and styling in a form that can be pushed to
the Google Slides API.

## Details

You should not construct `r2slides_table` objects directly. Instead, use
[`as_r2slides_table()`](https://adam-hubbs.github.io/r2slides/reference/as_r2slides_table.md)
to convert a supported table object (e.g. a **flextable**) into one.
