# Execute buffered API requests

EXPERIMENTAL. Executes all pending requests in the buffer. Each request
is removed from the buffer after it is attempted, whether or not it
succeeds. Typically called after accumulating requests with the `"lazy"`
evaluation strategy (see
[`set_evaluation_strategy()`](https://adam-hubbs.github.io/r2slides/reference/set_evaluation_strategy.md)).

## Usage

``` r
execute_requests(batch_all = TRUE)
```

## Arguments

- batch_all:

  Logical. If `TRUE` (the default), all pending
  `slides.presentations.batchUpdate` requests for the same presentation
  are merged into a single API call per presentation. Other request
  types are executed individually. If `FALSE`, every buffered request is
  executed one at a time in the order it was added.

## Value

`NULL`, invisibly.

## See also

[`set_evaluation_strategy()`](https://adam-hubbs.github.io/r2slides/reference/set_evaluation_strategy.md),
[`view_request_buffer()`](https://adam-hubbs.github.io/r2slides/reference/view_request_buffer.md)
