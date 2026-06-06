# Match elements by type and position

Returns a match function that finds an existing slide element with the
same type whose centroid falls within `tolerance` inches of the new
element's centroid. If multiple elements match, a warning is issued and
`NULL` is returned (no replacement is made).

## Usage

``` r
match_by_type_and_position(tolerance = 0.05)
```

## Arguments

- tolerance:

  Numeric. Maximum distance in inches between centroids for a match to
  be accepted. Default: `0.05`.

## Value

A function with signature
`function(new_spec, existing_elements) -> character(1) | NULL`.
`new_spec` is a named list with `type`, `left`, `top`, `width`,
`height`. `existing_elements` is a tibble with those same columns plus
`element_id`.

## See also

[`set_match_fn()`](https://adam-hubbs.github.io/r2slides/reference/set_match_fn.md),
[`get_match_fn()`](https://adam-hubbs.github.io/r2slides/reference/get_match_fn.md),
[`set_replacement_strategy()`](https://adam-hubbs.github.io/r2slides/reference/set_replacement_strategy.md)
