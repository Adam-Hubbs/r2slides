# Takes a slide position object, applies transformations to its dimensions, and returns a new slide_position object. Transformations can be either functions or scalar numeric values. If a scalar is provided, it replaces the original value. If a function is provided, it's applied to the original value.

Takes a slide position object, applies transformations to its
dimensions, and returns a new slide_position object. Transformations can
be either functions or scalar numeric values. If a scalar is provided,
it replaces the original value. If a function is provided, it's applied
to the original value.

## Usage

``` r
relative_annotation(
  position,
  top_transformation = identity,
  left_transformation = identity,
  width_transformation = identity,
  height_transformation = identity
)
```

## Arguments

- position:

  An object of class \`r2slides::slide_position\`

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

A new object of class \`r2slides::slide_position\`

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a base position
base_pos <- slide_position(left = 1, top = 2, width = 5, height = 3)

# Move right by 0.5 inches using a function, set the maximum width at 5
new_pos <- relative_annotation(
  base_pos,
  top_transformation = identity,
  left_transformation = function(x) x + 0.5,
  width_transformation = \(x) min(x, 5),
  height_transformation = identity
)

# Set absolute position using scalars
new_pos2 <- relative_annotation(
  base_pos,
  top_transformation = 3,
  left_transformation = 2,
  width_transformation = identity,
  height_transformation = identity
)
} # }
```
