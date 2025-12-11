# Add a title to a Google Slide

Add a title to a Google Slide

## Usage

``` r
add_title(
  text = "Title",
  presentation = google_presentation,
  position = NULL,
  title_font_size = NULL,
  title_font_family = NULL,
  title_font_bold = NULL,
  title_color = NULL,
  title_bg_color = NULL,
  report_style = c("qualtrics", "municipal", "y2"),
  verbose = TRUE,
  token = NULL,
  call = rlang::caller_env(),
  ...
)
```

## Arguments

- text:

  A single string for the title text. Optional, defaults to "Title".

- presentation:

  Optional. A Google Slides presentation object. By default it searches
  for an environment named \`google_presentation\`.

- position:

  An object of class \`r2slides::slide_position\`. If \`NULL\`, default
  position will be created based on \`report_style\`.

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

- report_style:

  One of \`"qualtrics"\`, \`"municipal"\`, or \`"y2"\`.

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

The element_id of the created title (invisibly)
