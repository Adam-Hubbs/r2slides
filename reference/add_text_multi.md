# Add or update text in a Google Slides presentation

Add or update text in a Google Slides presentation

## Usage

``` r
add_text_multi(
  slide_obj,
  text,
  position,
  position_base = NULL,
  element_id = NULL,
  text_style = NULL,
  verbose = TRUE,
  token = NULL,
  pass_strategy = c("one", "all"),
  debug = FALSE,
  ...
)
```

## Arguments

- slide_obj:

  A Google Slides slide object.

- text:

  A vector of character strings to add.

- position:

  A vector of objects of class \`r2slides::slide_position\`

- position_base:

  A vector of objects of class \`r2slides::slide_position\`

- element_id:

  Optional. A vector of string IDs of an existing text element to
  update. If element_id is \`NULL\` a new element will be created.

- text_style:

  Optional. A vector of text_style or style_rule objects.

- verbose:

  Optional. A logical indicating whether to print API responses.
  Default: TRUE.

- token:

  Optional. An OAuth2 token. The default uses \`r2slides_token()\` to
  find a token.

- pass_strategy:

  Optional. A strategy to pass additional values to style_rule objects.

- debug:

  Optional. A logical indicating whether to print debug messages.
  Default: FALSE.

- ...:

  Additional values available to style_rule objects.

## Value

The Google Slides slide object (invisibly).
