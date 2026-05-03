# Get the current request evaluation strategy

Returns the value of the \`r2slides_request_evaluation_strategy\`
environment variable, defaulting to \`"eager"\` if unset.

## Usage

``` r
get_evaluation_strategy()
```

## Value

A string, either \`"eager"\` or \`"lazy"\`.

## See also

\[set_evaluation_strategy()\]
