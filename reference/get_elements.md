# Get elements from a slide

Retrieves elements from a slide as a list of typed element objects, with
optional filtering by element type and/or spatial region.

## Usage

``` r
get_elements(slide, type = NULL, within = NULL, contains = c("any", "all"))
```

## Arguments

- slide:

  A `slide` object (from
  [`on_slide_id()`](https://adam-hubbs.github.io/r2slides/reference/on_slide_id.md),
  [`on_slide_number()`](https://adam-hubbs.github.io/r2slides/reference/on_slide_id.md),
  etc.).

- type:

  Optional character vector of element types to keep. Valid values:
  `"text"`, `"image"`, `"chart"`, `"table"`, `"shape"`, `"unknown"`. If
  `NULL` (default) all element types are returned.

- within:

  Optional. Either a `slide_position` object (a rectangular query
  region) or a numeric vector of length 2 `c(top, left)` in inches (a
  single point). For a point, elements whose bounding box contains the
  point are returned. If `NULL` (default), no spatial filtering is
  applied.

- contains:

  One of `"any"` (default) or `"all"`. Controls how an element's
  bounding box is compared to a `slide_position` `within` region:

  - `"any"`: element is kept if it *overlaps* the region.

  - `"all"`: element is kept only if it is *fully inside* the region.

  Ignored when `within` is a point.

## Value

A list of element objects (`text_element`, `image_element`, etc.).
Returns an empty list if no elements match the filters.

## Details

**Type filtering** (`type`): Supply one or more of `"text"`, `"image"`,
`"chart"`, `"table"`, `"shape"`, `"unknown"` to restrict the result to
those element types. Uses
[`element_type()`](https://adam-hubbs.github.io/r2slides/reference/element_type.md)
for classification.

**Spatial filtering** (`within` / `contains`): `within` can be either a
region or a single point.

- A `slide_position` defines a rectangular region. The `contains`
  argument then controls strictness:

  - `"any"` (default): keep elements whose bounding box *intersects* the
    region (any overlap).

  - `"all"`: keep elements whose bounding box is *fully contained*
    within the region (strict containment).

- A length-2 numeric vector `c(top, left)` in inches defines a single
  point. Elements whose bounding box *contains that point* are returned,
  and `contains` is ignored.

Rotation is accounted for when computing bounding boxes via
`slide_position`'s
[`bounding_box()`](https://adam-hubbs.github.io/r2slides/reference/bounding_box.md)
computed property.

## See also

[`get_all_elements()`](https://adam-hubbs.github.io/r2slides/reference/get_all_elements.md),
[`element_type()`](https://adam-hubbs.github.io/r2slides/reference/element_type.md),
[`get_raw()`](https://adam-hubbs.github.io/r2slides/reference/get_raw.md),
[`get_position()`](https://adam-hubbs.github.io/r2slides/reference/get_position.md),
[`get_text()`](https://adam-hubbs.github.io/r2slides/reference/get_text.md),
[`get_text_style()`](https://adam-hubbs.github.io/r2slides/reference/get_text_style.md)

## Examples

``` r
if (FALSE) {
  ps <- register_presentation(id = "YOUR_PRESENTATION_ID", set_active = FALSE)
  slide_obj <- on_slide_number(1, ps)

  # All elements
  all_els <- get_elements(slide_obj)

  # Text elements only
  text_els <- get_elements(slide_obj, type = "text")

  # Elements overlapping the top-left quadrant
  region <- slide_position(top = 0, left = 0, width = 5, height = 2.8)
  top_left_els <- get_elements(slide_obj, within = region)

  # Elements fully contained within the region
  inner_els <- get_elements(slide_obj, within = region, contains = "all")

  # Elements covering the point 2" down, 3" from the left
  at_point <- get_elements(slide_obj, within = c(2, 3))
}
```
