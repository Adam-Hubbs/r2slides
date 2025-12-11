# This function takes transformations for top, left, width, and height, and returns a function that applies those transformations to a slide_position object.

This function takes transformations for top, left, width, and height,
and returns a function that applies those transformations to a
slide_position object.

## Usage

``` r
define_relative_transformation_function(
  top_transformation = identity,
  left_transformation = identity,
  width_transformation = identity,
  height_transformation = identity
)
```

## Arguments

- top_transformation:

  A function to apply to the top value, or a scalar numeric to replace
  it

- left_transformation:

  A function to apply to the left value, or a scalar numeric to replace
  it

- width_transformation:

  A function to apply to the width value, or a scalar numeric to replace
  it

- height_transformation:

  A function to apply to the height value, or a scalar numeric to
  replace it

## Value

A function that applies those transformations to a slide_position
object.
