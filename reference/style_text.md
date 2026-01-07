# Apply text styling to an existing text element

Apply text styling to an existing text element

## Usage

``` r
style_text(slide_obj, element_id, style, token = NULL, call = NULL)
```

## Arguments

- slide_obj:

  A Google Slides slide object.

- element_id:

  The object ID of the text element.

- style:

  A list of text styling properties.

- token:

  Optional. An OAuth2 token. The default uses \`r2slides_token()\` to
  find a token.

- call:

  Optional. Call environment used in error messages.
