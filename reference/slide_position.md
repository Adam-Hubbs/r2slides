# Slide Position Object

A system for working with Google Slides positions and sizes.

## Usage

``` r
slide_position(
  top,
  left,
  width,
  height,
  convert_slide_size = FALSE,
  slide_size_old = NULL,
  slide_size = c(5.625, 10)
)
```

## Arguments

- top:

  The position in inches from the top of the slide

- left:

  The position in inches from the left of the slide

- width:

  The width of the object in inches

- height:

  The height of the object in inches

- convert_slide_size:

  A logical. Optional. Converts the position and size to Google Slides
  from a PowerPoint (or custom) size if TRUE.

- slide_size_old:

  Old Slide size specifications. Optional. Used for conversion between
  powerpoint and google slides sizes.

- slide_size:

  New Slide size specifications. Optional.
