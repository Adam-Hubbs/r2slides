# Add a footer to a Google Slide

Add a footer to a Google Slide

## Usage

``` r
add_footer(
  text = "Footer",
  presentation = google_presentation,
  position = NULL,
  footer_font_size = 10,
  footer_font_family = NULL,
  footer_color = NULL,
  report_style = c("qualtrics", "municipal", "y2"),
  bg_color = NULL,
  verbose = TRUE,
  token = NULL,
  call = rlang::caller_env(),
  ...
)
```

## Arguments

- text:

  A string. Optional, defaults to "Footer".

- presentation:

  A Google Slides presentation object.

- position:

  An object of class \`r2slides::slide_position\`. If \`NULL\`, default
  position will be created based on \`report_style\`.

- footer_font_size:

  A numeric value for font size in points. Optional, defaults to 10.

- footer_font_family:

  A string specifying the font family. Optional.

- footer_color:

  A string specifying the color (hex code). Optional.

- report_style:

  One of \`"qualtrics"\`, \`"municipal"\`, or \`"y2"\`.

- bg_color:

  A string specifying background color. Optional.

- verbose:

  Optional. A logical indicating whether to print API responses.
  Default: \`TRUE\`.

- token:

  Optional. An OAuth2 token. The default uses \`r2slides_token()\` to
  find a token.

- call:

  Optional. Call environment used in error messages.

- ...:

  Additional arguments reserved for future expansion.

## Value

The element_id of the created footer (invisibly)
