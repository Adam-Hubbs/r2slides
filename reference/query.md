# Query Google API's

Query Google API's

## Usage

``` r
query(
  endpoint = "slides.presentations.batchUpdate",
  params = list(),
  body = NULL,
  base = NULL,
  token = NULL,
  debug = FALSE,
  max_tries = 4L,
  backoff_base = 3,
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

- debug:

  Optional. If \`TRUE\`, return the unexecuted request. If \`FALSE\`,
  execute the request.\` Default: \`FALSE\`.

- max_tries:

  Optional. Maximum number of attempts before giving up. Default: \`4\`.
  Set to \`1\` to disable retrying.

- backoff_base:

  Optional. Base (in seconds) for truncated exponential backoff.
  Default: \`3\`. Wait time per attempt is \`min(backoff_base ^
  attempt, 60) + runif(1, 0, 1)\` seconds.

- call:

  Optional. Call environment used in error messages.

- ...:

  Additional arguments reserved for future expansion.

## Value

A processed API response. Will error if the endpoint is not recognized
or if required parameters are missing or malformed.
