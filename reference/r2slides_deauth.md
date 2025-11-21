# Clear current token

If you ever get an error saying client error: (401) UNAUTHENTICATED with
a warning message of \`Unable to refresh token: invalid_grant\`, then
run this function, and restart the R Session. It should restart the auth
process next time you need it.

## Usage

``` r
r2slides_deauth()
```
