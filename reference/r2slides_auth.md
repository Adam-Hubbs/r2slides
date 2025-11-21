# Create OAuth authentication token

Create OAuth authentication token

## Usage

``` r
r2slides_auth(
  email = gargle::gargle_oauth_email(),
  scopes = c("https://www.googleapis.com/auth/spreadsheets",
    "https://www.googleapis.com/auth/presentations",
    "https://www.googleapis.com/auth/drive"),
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default()
)
```

## Arguments

- email:

  Optional. A string giving an email address that can be used to
  identify the user.

- scopes:

  Optional. A character vector of requested OAuth scopes.

- cache:

  Optional. Specifies the OAuth token cache.

- use_oob:

  Optional. Whether to use out-of-band authentication.

## Value

Nothing directly. Updates the .auth token. Will error if unable to
retrieve a valid OAuth2.0 token.
