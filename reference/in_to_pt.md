# Convert inches to points

Google slides API's can only handle EMU or PT's. This function converts
inches to PT's so the user can continue to work in inches.

## Usage

``` r
in_to_pt(x)
```

## Arguments

- x:

  A numeric vector of measurements in inches.

## Value

A numeric vector of measurements in points (1/72 inches). Errors if
non-numeric input is provided.
