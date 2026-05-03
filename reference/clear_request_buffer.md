# Clear the request buffer

Removes all entries from the internal request buffer, including any that
have already been executed. Useful for resetting state between
workflows.

## Usage

``` r
clear_request_buffer()
```

## Value

\`NULL\`, invisibly.

## See also

\[view_request_buffer()\], \[execute_requests()\]
