# Clear the request buffer

Removes all entries from the internal request buffer. Useful for
resetting state between workflows.

## Usage

``` r
clear_request_buffer()
```

## Value

`NULL`, invisibly.

## See also

[`view_request_buffer()`](https://adam-hubbs.github.io/r2slides/reference/view_request_buffer.md),
[`execute_requests()`](https://adam-hubbs.github.io/r2slides/reference/execute_requests.md)
