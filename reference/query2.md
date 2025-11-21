# Query Google API's

Query Google API's

## Usage

``` r
query2(
  endpoint = "slides.presentations.batchUpdate",
  params = list(),
  body = NULL,
  base = NULL,
  token = NULL,
  call = rlang::caller_env(),
  ...
)
```

## Arguments

- endpoint:

  A single string indicating the API endpoint.

- params:

  Optional. A list of parameters for the API request.

- body:

  Optional. A request body.

- base:

  Optional. A string indicating the API base service name. (i.e.
  'slides', or 'sheets')

- token:

  Optional. An OAuth2 token. The default uses \`r2slides_token()\` to
  find a token.

- call:

  Optional. Call environment used in error messages.

- ...:

  Additional arguments reserved for future expansion.

## Value

A processed API response. Will error if the endpoint is not recognized
or if required parameters are missing or malformed.
