# Add a title to a Google Slide

Add a title to a Google Slide

## Usage

``` r
add_title(
  text = "Title",
  presentation = google_presentation,
  title_font_size = NULL,
  title_font_family = NULL,
  title_font_bold = NULL,
  title_color = NULL,
  title_bg_color = NULL,
  title_left = NULL,
  title_top = NULL,
  title_width = NULL,
  title_height = NULL,
  report_style = c("qualtrics", "municipal", "y2"),
  verbose = TRUE,
  convert_slide_size = TRUE,
  slide_size = NULL,
  ...
)
```

## Arguments

- text:

  A single string for the title text. Optional, defaults to "Title".

- presentation:

  Optional. A Google Slides presentation object. By default it searches
  for an environment named \`google_presentation\`.

- title_font_size:

  Optional. Font size in points.

- title_font_family:

  Optional. Font family name.

- title_font_bold:

  Optional. Whether text should be bold.

- title_color:

  Optional. Title text color as hex code.

- title_bg_color:

  Optional. Title background color as hex code.

- title_left:

  Optional. Left position of title.

- title_top:

  Optional. Top position of title.

- title_width:

  Optional. Width of title element.

- title_height:

  Optional. Height of title element.

- report_style:

  One of \`"qualtrics"\`, \`"municipal"\`, or \`"y2"\`.

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

The element_id of the created title (invisibly)
