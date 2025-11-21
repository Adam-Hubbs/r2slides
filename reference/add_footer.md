# Add a footer to a Google Slide

Add a footer to a Google Slide

## Usage

``` r
add_footer(
  text = "Footer",
  presentation = google_presentation,
  footer_font_size = 10,
  footer_font_family = NULL,
  footer_color = NULL,
  footer_left = NULL,
  footer_top = NULL,
  footer_width = NULL,
  footer_height = NULL,
  report_style = c("qualtrics", "municipal", "y2"),
  bg_color = NULL,
  verbose = TRUE,
  convert_slide_size = TRUE,
  slide_size = NULL,
  ...
)
```

## Arguments

- text:

  A string. Optional, defaults to "Footer".

- presentation:

  A Google Slides presentation object.

- footer_font_size:

  A numeric value for font size in points. Optional, defaults to 10.

- footer_font_family:

  A string specifying the font family. Optional.

- footer_color:

  A string specifying the color (hex code). Optional.

- footer_left:

  A numeric value for left position. Optional.

- footer_top:

  A numeric value for top position. Optional.

- footer_width:

  A numeric value for width. Optional.

- footer_height:

  A numeric value for height. Optional.

- report_style:

  One of \`"qualtrics"\`, \`"municipal"\`, or \`"y2"\`.

- bg_color:

  A string specifying background color. Optional.

- verbose:

  Optional. A logical indicating whether to print API responses.
  Default: \`TRUE\`.

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

The element_id of the created footer (invisibly)
