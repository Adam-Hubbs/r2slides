# Set the replacement strategy

Controls how `add_*` functions behave when an existing element on the
slide matches the new one (as determined by the active match function).

## Usage

``` r
set_replacement_strategy(strategy = c("add", "replace", "skip"))
```

## Arguments

- strategy:

  One of `"add"`, `"replace"`, or `"skip"`.

## Value

The strategy string, invisibly.

## Details

- `"add"` (default): always create a new element; match function is
  never called.

- `"replace"`: delete the matched element, then create a fresh one. If
  no match is found, a new element is created.

- `"skip"`: leave the matched element untouched and skip creation. If no
  match is found, a new element is created.

## See also

[`get_replacement_strategy()`](https://adam-hubbs.github.io/r2slides/reference/get_replacement_strategy.md),
[`set_match_fn()`](https://adam-hubbs.github.io/r2slides/reference/set_match_fn.md),
[`match_by_type_and_position()`](https://adam-hubbs.github.io/r2slides/reference/match_by_type_and_position.md)
