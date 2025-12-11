# Add commentary text to a Google Slide

Add commentary text to a Google Slide

## Usage

``` r
add_commentary(
  text = "Commentary",
  presentation = google_presentation,
  position = NULL,
  commentary_font_size = 14,
  commentary_font_family = NULL,
  commentary_color = NULL,
  commentary_bg_color = NULL,
  report_style = c("qualtrics", "municipal", "y2"),
  verbose = TRUE,
  token = NULL,
  call = rlang::caller_env(),
  ...
)
```

## Arguments

- text:

  A single string. The text to add as commentary.

- presentation:

  A Google Slides presentation object.

- position:

  An object of class \`r2slides::slide_position\`. If \`NULL\`, default
  position will be created based on \`report_style\`.

- commentary_font_size:

  A numeric value for font size in points. Optional.

- commentary_font_family:

  A single string specifying font family. Optional.

- commentary_color:

  A single string with hex color code for text. Optional.

- commentary_bg_color:

  A single string with hex color code for background. Optional.

- report_style:

  One of \`"qualtrics"\`, \`"municipal"\`, or \`"y2"\`.

- verbose:

  Optional. A logical indicating whether to print API responses.
  Default: TRUE.

- token:

  Optional. An OAuth2 token. The default uses \`r2slides_token()\` to
  find a token.

- call:

  Optional. Call environment used in error messages.

- ...:

  Additional arguments reserved for future expansion.

## Value

The element_id of the commentary text (invisibly)
