# Calculate Bounding Box for Slide Position Objects

Returns the axis-aligned bounding box that contains one or more
slide_position objects, accounting for rotation.

## Usage

``` r
bounding_box(...)
```

## Arguments

- ...:

  One or more slide_position objects

## Value

A slide_position object representing the bounding box (with rotation =
0)

## Details

When objects are rotated, their actual occupied space extends beyond
their intrinsic width and height. This function calculates the smallest
axis-aligned rectangle that contains all provided objects, including any
rotation.

All objects must have the same slide_size (slide_width and
slide_height).
