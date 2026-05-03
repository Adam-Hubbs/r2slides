# View the pending request buffer

Returns a tibble showing all buffered API requests. Each row represents
one deferred \[query()\] call. Useful for inspecting what is queued when
using the \`"lazy"\` evaluation strategy.

## Usage

``` r
view_request_buffer()
```

## Value

A \[tibble::tibble()\] with columns: - \`request\`: a list of query
arguments (endpoint, params, body, base, etc.) - \`time_requested\`:
when the request was buffered - \`tried_to_execute\`: whether execution
has been attempted - \`execute_succeeded\`: \`NA\` if not yet attempted,
\`TRUE\`/\`FALSE\` otherwise - \`presentation\`: the \`presentationId\`
from the request params, or \`NA\`

## See also

\[set_evaluation_strategy()\], \[execute_requests()\]
