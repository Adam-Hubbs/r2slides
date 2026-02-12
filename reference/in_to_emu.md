# Convert inches to EMU's

Google slides API's can only handle EMU or PT's. This function converts
inches to EMU's so the user can continue to work in inches.

## Usage

``` r
in_to_emu(x)
```

## Arguments

- x:

  A numeric vector of measurements in inches.

## Value

A numeric vector of measurements in english metric units (1/914,400
inches). Errors if non-numeric input is provided.
