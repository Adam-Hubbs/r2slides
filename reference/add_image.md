# Add image to a Google Slides presentation

Inserts an image onto a slide. Accepts a ggplot object, a local file
path, or a publicly accessible URL.

When given a ggplot or local file, the image is temporarily uploaded to
Google Drive, inserted into the slide, then deleted from Drive. The
image is permanently embedded in the presentation after insertion.

## Usage

``` r
add_image(
  slide_obj,
  image,
  position,
  fit = c("contain", "cover", "distort", "raw"),
  dpi = NULL,
  order = c("front", "back")
)
```

## Arguments

- slide_obj:

  A Google Slides slide object.

- image:

  A ggplot object, a local file path, or a publicly accessible URL
  string. If an image is specified, it must be in one of the following
  formats:

  - png

  - jpg

  - gif

- position:

  An object of class
  [`r2slides::slide_position`](https://adam-hubbs.github.io/r2slides/reference/slide_position.md).

- fit:

  Controls how the image maps onto the `position` bounding box. One of:

  - `"contain"` (default): scale to fit entirely within the bounding
    box, preserving aspect ratio, centered.

  - `"cover"`: fill the bounding box completely, clipping overflow from
    the center, preserving aspect ratio.

  - `"distort"`: stretch to fill exactly, ignoring aspect ratio.

  - `"raw"`: place at the image's natural pixel size at the given `dpi`,
    anchored to the top-left of `position`. Overflow is allowed.

  All modes except `"distort"` require the `magick` package to read
  image dimensions.

- dpi:

  Numeric. Only used when `fit = "raw"`. The DPI to use when converting
  pixels to inches. `NULL` auto-detects from the file metadata, falling
  back to 96.

- order:

  Optional. One of `"front"` or `"back"`. Controls the Z-order of the
  created element. Default: `"front"`.

## Value

The Google Slides slide object (invisibly).

## Examples

``` r
if(FALSE) {
plot <- ggplot(mtcars, aes(x = cyl, y = hp)) +
           geom_point()

  on_slide_number(4) |>
    add_image(plot)
}
```
