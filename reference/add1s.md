# Create a slide with title, commentary, and footer

Create a slide with title, commentary, and footer

## Usage

``` r
add1s(
  presentation = google_presentation,
  layout = "BLANK",
  title = "Title",
  commentary = "Commentary",
  footer = "Footer",
  report_style = c("qualtrics", "municipal", "y2"),
  verbose = TRUE,
  convert_slide_size = TRUE,
  slide_size = NULL,
  title_font_size = NULL,
  title_font_family = NULL,
  title_font_bold = NULL,
  title_color = NULL,
  title_bg_color = NULL,
  title_left = NULL,
  title_top = NULL,
  title_width = NULL,
  title_height = NULL,
  commentary_font_size = 14,
  commentary_font_family = NULL,
  commentary_color = NULL,
  commentary_bg_color = NULL,
  commentary_left = NULL,
  commentary_top = NULL,
  commentary_width = NULL,
  commentary_height = NULL,
  footer_font_size = 10,
  footer_font_family = NULL,
  footer_color = NULL,
  footer_left = NULL,
  footer_top = NULL,
  footer_width = NULL,
  footer_height = NULL,
  ...
)
```

## Arguments

- presentation:

  Optional. A Google Slides presentation object. By default it searches
  for an environment named \`google_presentation\`.

- layout:

  One of the available Google Slides layouts. Optional.

- title:

  A single string for the slide title. Optional.

- commentary:

  A single string for the slide commentary. Optional.

- footer:

  A single string for the slide footer. Optional.

- report_style:

  One of \`"qualtrics"\`, \`"municipal"\`, or \`"y2"\`. Defaults to
  \`qualtrics\`.

- verbose:

  A logical indicating whether to print progress messages. Optional.

- convert_slide_size:

  A logical. Optional.

- slide_size:

  Slide size specifications. Optional.

- title_font_size:

  Title font size. Optional.

- title_font_family:

  Title font family. Optional.

- title_font_bold:

  Title font weight. Optional.

- title_color:

  Title text color. Optional.

- title_bg_color:

  Title background color. Optional.

- title_left:

  Title left position. Optional.

- title_top:

  Title top position. Optional.

- title_width:

  Title width. Optional.

- title_height:

  Title height. Optional.

- commentary_font_size:

  Commentary font size. Optional, defaults to 14.

- commentary_font_family:

  Commentary font family. Optional.

- commentary_color:

  Commentary text color. Optional.

- commentary_bg_color:

  Commentary background color. Optional.

- commentary_left:

  Commentary left position. Optional.

- commentary_top:

  Commentary top position. Optional.

- commentary_width:

  Commentary width. Optional.

- commentary_height:

  Commentary height. Optional.

- footer_font_size:

  Footer font size. Optional, defaults to 10.

- footer_font_family:

  Footer font family. Optional.

- footer_color:

  Footer text color. Optional.

- footer_left:

  Footer left position. Optional.

- footer_top:

  Footer top position. Optional.

- footer_width:

  Footer width. Optional.

- footer_height:

  Footer height. Optional.

- ...:

  Additional arguments reserved for future expansion.

## Value

Updates the presentation object with the new slide ID, invisibly.
