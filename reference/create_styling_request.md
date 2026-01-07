# Create a styling request

Create a styling request

## Usage

``` r
create_styling_request(
  style_rule,
  text,
  element_id,
  ...,
  call = rlang::caller_env()
)
```

## Arguments

- style_rule:

  A style_rule object

- text:

  A character string of text

- element_id:

  A string ID of an existing text element to update. If element_id is
  \`NULL\` a new element will be created.

- ...:

  Data to be avaiable in the data mask where the selector function will
  be executed. Must be named.

- call:

  Optional. Call environment used in error messages.

## Value

A list of Google Slides styling requests
