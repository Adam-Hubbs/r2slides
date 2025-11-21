# Add or update text in a Google Slides presentation

Add or update text in a Google Slides presentation

## Usage

``` r
add_text(
  text,
  presentation = google_presentation,
  element_id = NULL,
  left = 1,
  top = 1,
  width = 2,
  height = 0.5,
  text_style = NULL,
  verbose = TRUE,
  report_style = c("qualtrics", "municipal", "y2"),
  bg_color = NULL,
  convert_slide_size = TRUE,
  slide_size = NULL,
  ...
)
```

## Arguments

- text:

  A character string of text to add.

- presentation:

  Optional. A Google Slides presentation object. By default it searches
  for an environment named \`google_presentation\`.

- element_id:

  Optional. A string ID of an existing text element to update. If
  element_id is \`NULL\` a new element will be created.

- left:

  Optional. A numeric position from left edge in inches. Default: 1.

- top:

  Optional. A numeric position from top edge in inches. Default: 1.

- width:

  Optional. A numeric width in inches. Default: 2.

- height:

  Optional. A numeric height in inches. Default: 0.5.

- text_style:

  Optional. A list of text styling properties.

- verbose:

  Optional. A logical indicating whether to print API responses.
  Default: TRUE.

- report_style:

  One of \`"qualtrics"\`, \`"municipal"\`, or \`"y2"\`.

- bg_color:

  Optional. A list specifying background color (RGB).

- convert_slide_size:

  Optional. A logical indicating whether to convert dimensions from
  PowerPoint into Google Slides. If \`TRUE\`, then positioning would
  work identically to a PowerPoint presentation. If \`FALSE\` it uses
  the raw inches directly. Default: TRUE.

- slide_size:

  Optional. A list containing slide size specifications for converting.
  In the form of list(x_height = 7.5, x_width = 13.3, y_height = 5,
  y_width = 9). \`x\_\` indicates converting from and \`y\_\` indicates
  converting too. You can override this to convert between custom slide
  sizes. If left blank it converts from PowerPoint to Google Slides.

- ...:

  Additional arguments reserved for future expansion.

## Value

The element ID invisibly. Will error if no slide is currently selected
or if text is not a character string.
