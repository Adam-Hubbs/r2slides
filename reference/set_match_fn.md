# Set the match function

Sets the package-wide function used to find an existing slide element
that matches the one being added. Used by `add_*` functions when the
replacement strategy is `"replace"` or `"skip"`.

## Usage

``` r
set_match_fn(fn)
```

## Arguments

- fn:

  A function with the signature described above.

## Value

`fn`, invisibly.

## Details

The function must accept two arguments:

- `new_spec`: a named list with `type` (character), `left`, `top`,
  `width`, `height` (numeric, in inches).

- `existing_elements`: a tibble with the same columns plus `element_id`.

It must return either a single `element_id` string or `NULL`. If
multiple elements match, the function should warn and return `NULL`.

## See also

[`get_match_fn()`](https://adam-hubbs.github.io/r2slides/reference/get_match_fn.md),
[`match_by_type_and_position()`](https://adam-hubbs.github.io/r2slides/reference/match_by_type_and_position.md)
