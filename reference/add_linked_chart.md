# Add a linked Google Sheets chart to a Google Slides presentation

Add a linked Google Sheets chart to a Google Slides presentation

## Usage

``` r
add_linked_chart(
  chart_obj,
  slide_obj,
  position,
  linked = TRUE,
  token = NULL,
  call = rlang::caller_env()
)
```

## Arguments

- chart_obj:

  A Google Sheets chart object

- slide_obj:

  A Google Slides slide object

- position:

  An object of class \`r2slides::slide_position\`

- linked:

  Optional. A logical indicating whether the chart should be linked.
  Default: \`TRUE\`.

- token:

  Optional. An OAuth2 token. The default uses \`r2slides_token()\` to
  find a token.

- call:

  Optional. Call environment used in error messages.

## Value

The Google Slides slide object (invisibly)
