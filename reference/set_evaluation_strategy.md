# Set the request evaluation strategy

Controls whether API requests are executed immediately (\`"eager"\`, the
default) or buffered for later batch execution (\`"lazy"\`). When
\`"lazy"\`, requests accumulate in an internal buffer until
\[execute_requests()\] is called.

## Usage

``` r
set_evaluation_strategy(strategy = c("eager", "lazy"))
```

## Arguments

- strategy:

  One of \`"eager"\` (default) or \`"lazy"\`.

## Value

The strategy string, invisibly.

## See also

\[get_evaluation_strategy()\], \[execute_requests()\],
\[view_request_buffer()\]
