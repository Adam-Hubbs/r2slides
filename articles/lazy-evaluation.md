# Lazy Evaluation and Request Batching

``` r

library(r2slides)
```

By default, r2slides executes every API request immediately as you call
functions like
[`add_text()`](https://adam-hubbs.github.io/r2slides/reference/add_text.md),
[`add_linked_chart()`](https://adam-hubbs.github.io/r2slides/reference/add_linked_chart.md),
or
[`query()`](https://adam-hubbs.github.io/r2slides/reference/query.md).
This is called **eager** evaluation and is the simplest mental model:
each call maps to one API round-trip.

r2slides also supports a **lazy** evaluation strategy. In lazy mode,
requests are captured in an in-memory buffer instead of being sent to
Google immediately. You choose when to flush the buffer by calling
[`execute_requests()`](https://adam-hubbs.github.io/r2slides/reference/execute_requests.md).
This lets you build up a batch of changes and send them all at once,
which is useful when:

- You are constructing many slides programmatically and want to minimise
  latency.
- You want to inspect what will be sent before actually sending it.
- You need to combine a large number of individual requests into a
  single Google Slides `batchUpdate` call to reduce API quota
  consumption.

## Setting the evaluation strategy

Use
[`set_evaluation_strategy()`](https://adam-hubbs.github.io/r2slides/reference/set_evaluation_strategy.md)
to switch modes and
[`get_evaluation_strategy()`](https://adam-hubbs.github.io/r2slides/reference/get_evaluation_strategy.md)
to check the current setting. The strategy is stored in the
`r2slides_request_evaluation_strategy` environment variable.

``` r

set_evaluation_strategy("lazy")   # buffer requests
get_evaluation_strategy()          # "lazy"

set_evaluation_strategy("eager")   # restore default
get_evaluation_strategy()          # "eager"
```

You can also set the environment variable directly before starting R,
which is useful in `.Renviron` files for persistent configuration:

    r2slides_request_evaluation_strategy=lazy

## Buffering requests

Once lazy mode is active, every
[`query()`](https://adam-hubbs.github.io/r2slides/reference/query.md)
call (and every `add_*` function that calls it internally) appends a row
to an internal request buffer instead of making an HTTP request.

``` r

set_evaluation_strategy("lazy")

presentation <- register_presentation("My Presentation")
slide <- on_slide_number(1)

add_text("Hello", slide, in_top_left())
add_text("World", slide, in_top_right())
```

No API calls have been made. Both requests are sitting in the buffer.

## Inspecting the buffer

[`view_request_buffer()`](https://adam-hubbs.github.io/r2slides/reference/view_request_buffer.md)
returns a tibble with one row per buffered request:

``` r

view_request_buffer()
#> # A tibble: 2 × 5
#>   request   time_requested      tried_to_execute execute_succeeded presentation
#>   <list>    <dttm>              <lgl>            <lgl>             <chr>
#> 1 <named l> 2026-04-30 10:00:00 FALSE            NA                abc123...
#> 2 <named l> 2026-04-30 10:00:01 FALSE            NA                abc123...
```

Column descriptions:

| Column | Description |
|----|----|
| `request` | Named list of the raw [`query()`](https://adam-hubbs.github.io/r2slides/reference/query.md) arguments (endpoint, params, body, base, etc.) |
| `time_requested` | When the request was added to the buffer |
| `tried_to_execute` | `FALSE` until [`execute_requests()`](https://adam-hubbs.github.io/r2slides/reference/execute_requests.md) has attempted this row |
| `execute_succeeded` | `NA` before execution; `TRUE` on success, `FALSE` on failure |
| `presentation` | The `presentationId` from the request params, or `NA` |

## Executing buffered requests

Call
[`execute_requests()`](https://adam-hubbs.github.io/r2slides/reference/execute_requests.md)
to send all pending rows (those where `tried_to_execute` is `FALSE`).

### One at a time (`batch_all = FALSE`)

Each buffered request is dispatched in the order it was added, one API
call per row:

``` r

execute_requests(batch_all = FALSE)
```

This mirrors eager mode exactly — it is useful when the order of
individual operations matters and you do not need to optimise API quota
usage.

### Batched by presentation (`batch_all = TRUE`, the default)

When `batch_all = TRUE`, r2slides groups all pending
`slides.presentations.batchUpdate` requests that target the same
presentation and merges their request bodies into a single API call:

``` r

execute_requests(batch_all = TRUE)
```

For example, if you have ten
[`add_text()`](https://adam-hubbs.github.io/r2slides/reference/add_text.md)
calls for the same presentation buffered, `batch_all = TRUE` sends them
as one `batchUpdate` request containing all ten sub-requests, rather
than ten separate HTTP calls. This reduces both latency and API quota
usage.

Requests targeting different presentations are still sent as separate
calls (one per presentation). Any buffered request that is not a
`batchUpdate` (such as GET requests used to read presentation state) is
always executed individually, in its original order relative to the
batch.

After
[`execute_requests()`](https://adam-hubbs.github.io/r2slides/reference/execute_requests.md)
returns, check the buffer to see which requests succeeded:

``` r

view_request_buffer()
#> # A tibble: 2 × 5
#>   request   time_requested      tried_to_execute execute_succeeded presentation
#>   <list>    <dttm>              <lgl>            <lgl>             <chr>
#> 1 <named l> 2026-04-30 10:00:00 TRUE             TRUE              abc123...
#> 2 <named l> 2026-04-30 10:00:01 TRUE             TRUE              abc123...
```

Rows where `execute_succeeded` is `FALSE` indicate API errors. A warning
is emitted for each failure describing what went wrong. Failed rows are
not automatically retried — call
[`execute_requests()`](https://adam-hubbs.github.io/r2slides/reference/execute_requests.md)
again after fixing any underlying issue, and only the rows still marked
`tried_to_execute = FALSE` will be re-attempted.

## Clearing the buffer

[`clear_request_buffer()`](https://adam-hubbs.github.io/r2slides/reference/clear_request_buffer.md)
removes all rows, including those already executed:

``` r

clear_request_buffer()
nrow(view_request_buffer())  # 0
```

## Typical workflow

``` r

library(r2slides)

# 1. Register the presentation you want to modify.
presentation <- register_presentation("Quarterly Report")

# 2. Switch to lazy mode before building up changes.
set_evaluation_strategy("lazy")

# 3. Queue up all your changes — no API calls happen yet.
slide3 <- on_slide_number(3)
add_text("Q1 Results",     slide3, in_top_left())
add_text("Revenue: $1.2M", slide3, in_top_middle())
add_text("Growth: +12%",   slide3, in_top_right())

slide4 <- on_slide_number(4)
add_text("Q2 Outlook",     slide4, in_top_left())
add_text("Revenue: $1.4M", slide4, in_top_middle())

# 4. Inspect before committing (optional).
view_request_buffer()

# 5. Execute — all five requests are merged into one batchUpdate per presentation.
execute_requests(batch_all = TRUE)

# 6. Restore eager mode for interactive work.
set_evaluation_strategy("eager")
```
