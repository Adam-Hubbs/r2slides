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
  fit = c("fill", "natural"),
  dpi = NULL,
  order = c("front", "back"),
  replacement_strategy = get_replacement_strategy(),
  match_fn = get_match_fn()
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

  Controls how the image is sized on the slide. One of:

  - `"fill"` (default): the image occupies exactly the `position`
    bounding box, ignoring aspect ratio. For ggplot objects, the plot is
    rendered directly at the target dimensions. For local files, the
    `magick` package is required to resize the image before upload.

  - `"natural"`: the image is placed at its true pixel dimensions
    (converted to inches using `dpi`), anchored to the top-left corner
    of `position`. The image may overflow the bounding box. Requires the
    `magick` package.

- dpi:

  Numeric. Only used when `fit = "natural"`. The DPI to use when
  converting pixels to inches. `NULL` auto-detects from the file
  metadata, falling back to 96.

- order:

  Optional. One of `"front"` or `"back"`. Controls the Z-order of the
  created element. Default: `"front"`.

- replacement_strategy:

  One of `"add"` (default), `"replace"`, or `"skip"`. Overrides the
  package-wide default set by
  [`set_replacement_strategy()`](https://adam-hubbs.github.io/r2slides/reference/set_replacement_strategy.md).
  `"add"` always creates a new element. `"replace"` deletes any matching
  element and creates a fresh one. `"skip"` leaves a matching element
  untouched and suppresses creation.

- match_fn:

  A function that identifies an existing slide element matching the new
  one. Must have signature
  `function(new_spec, existing_elements) -> character(1) | NULL`. See
  [`match_by_type_and_position()`](https://adam-hubbs.github.io/r2slides/reference/match_by_type_and_position.md)
  for details and the built-in implementation. Overrides the
  package-wide default set by
  [`set_match_fn()`](https://adam-hubbs.github.io/r2slides/reference/set_match_fn.md).

## Value

The Google Slides slide object (invisibly).

## Examples

``` r
if(FALSE) {
plot <- ggplot(mtcars, aes(x = cyl, y = hp)) +
           geom_point()

  on_slide_number(4) |>
    add_image(plot, position = in_top_left())
}
```
