# Add commentary text to a Google Slide

Add commentary text to a Google Slide

## Usage

``` r
add_commentary(
  text = "Commentary",
  presentation = google_presentation,
  commentary_font_size = 14,
  commentary_font_family = NULL,
  commentary_color = NULL,
  commentary_bg_color = NULL,
  commentary_left = NULL,
  commentary_top = NULL,
  commentary_width = NULL,
  commentary_height = NULL,
  report_style = c("qualtrics", "municipal", "y2"),
  verbose = TRUE,
  convert_slide_size = TRUE,
  slide_size = NULL,
  ...
)
```

## Arguments

- text:

  A single string. The text to add as commentary.

- presentation:

  A Google Slides presentation object.

- commentary_font_size:

  A numeric value for font size in points. Optional.

- commentary_font_family:

  A single string specifying font family. Optional.

- commentary_color:

  A single string with hex color code for text. Optional.

- commentary_bg_color:

  A single string with hex color code for background. Optional.

- commentary_left:

  A numeric value for left position. Optional.

- commentary_top:

  A numeric value for top position. Optional.

- commentary_width:

  A numeric value for width. Optional.

- commentary_height:

  A numeric value for height. Optional.

- report_style:

  One of \`"qualtrics"\`, \`"municipal"\`, or \`"y2"\`.

- verbose:

  Optional. A logical indicating whether to print API responses.
  Default: TRUE.

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

The element_id of the commentary text (invisibly)
