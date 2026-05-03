# View the pending request buffer

Returns a tibble showing all buffered API requests. Each row represents
one deferred \[query()\] call. Useful for inspecting what is queued when
using the \`"lazy"\` evaluation strategy.

## Usage

``` r
view_request_buffer()
```

## Value

A \[tibble::tibble()\] with columns: - \`id\`: integer row identifier -
\`request\`: a list of query arguments (endpoint, params, body, base,
etc.) - \`time_requested\`: when the request was buffered -
\`resource_id\`: the resource identifier from the request params
(\`presentationId\`, \`spreadsheetId\`, or \`fileId\`), or \`NA\` -
\`user_call\`: the calling environment captured at buffer time, used for
error attribution on execution

## See also

\[set_evaluation_strategy()\], \[execute_requests()\]
