# Add or update text in a Google Slides presentation

Add or update text in a Google Slides presentation

## Usage

``` r
add_text(
  slide_obj,
  text,
  position,
  element_id = NULL,
  text_style = NULL,
  order = c("front", "back"),
  verbose = TRUE,
  token = NULL,
  debug = FALSE,
  ...
)
```

## Arguments

- slide_obj:

  A Google Slides slide object.

- text:

  A character string of text to add.

- position:

  An object of class \`r2slides::slide_position\`

- element_id:

  Optional. A string ID of an existing text element to update. If
  element_id is \`NULL\` a new element will be created.

- text_style:

  Optional. A list of text styling properties.

- order:

  Optional. One of \`"front"\` or \`"back"\`. Controls the Z-order of
  the created element. Default: \`"front"\`. Ignored when updating an
  existing element via \`element_id\`.

- verbose:

  Optional. A logical indicating whether to print API responses.
  Default: TRUE.

- token:

  Optional. An OAuth2 token. The default uses \`r2slides_token()\` to
  find a token.

- debug:

  Optional. A logical indicating whether to return the request objects,
  or evaluate them. Default: FALSE.

- ...:

  Additional values available to style_rule objects.

## Value

The Google Slides slide object (invisibly).
