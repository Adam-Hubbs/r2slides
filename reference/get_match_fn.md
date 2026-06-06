# Get the current match function

Returns the package-wide match function set by
[`set_match_fn()`](https://adam-hubbs.github.io/r2slides/reference/set_match_fn.md).
The default is
[`match_by_type_and_position()`](https://adam-hubbs.github.io/r2slides/reference/match_by_type_and_position.md)
with `tolerance = 0.05`.

## Usage

``` r
get_match_fn()
```

## Value

A function.

## See also

[`set_match_fn()`](https://adam-hubbs.github.io/r2slides/reference/set_match_fn.md),
[`match_by_type_and_position()`](https://adam-hubbs.github.io/r2slides/reference/match_by_type_and_position.md)
