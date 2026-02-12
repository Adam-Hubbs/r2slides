# Slide Position Object

A system for working with Google Slides positions and sizes.

## Usage

``` r
slide_position(
  top,
  left,
  width,
  height,
  rotation = 0,
  convert_slide_size = FALSE,
  slide_size_old = NULL,
  slide_size = c(5.625, 10)
)
```

## Arguments

- top:

  The position in inches from the top of the slide (top-left corner
  before rotation)

- left:

  The position in inches from the left of the slide (top-left corner
  before rotation)

- width:

  The width of the object in inches (intrinsic width, before rotation)

- height:

  The height of the object in inches (intrinsic height, before rotation)

- rotation:

  The rotation angle in degrees (default 0). Positive values rotate
  clockwise around the object's center.

- convert_slide_size:

  A logical. Optional. Converts the position and size to Google Slides
  from a PowerPoint (or custom) size if TRUE.

- slide_size_old:

  Old Slide size specifications. Optional. Used for conversion between
  powerpoint and google slides sizes.

- slide_size:

  New Slide size specifications. Optional.

## Details

When rotation is applied, the object rotates around its center point.
The \`top\`, \`left\`, \`width\`, and \`height\` properties describe the
object's intrinsic dimensions before rotation. The rotation is applied
as an affine transformation using the scaleX, scaleY, shearX, and shearY
computed properties.

To get the axis-aligned bounding box that contains the rotated object,
use the \`bounding_box()\` function.
