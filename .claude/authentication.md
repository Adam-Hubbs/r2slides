# Authentication & VCR Testing

How Google authentication and HTTP recording work in this package's test suite,
and how to diagnose/fix the common failures.

## How auth works in tests

- All Google API traffic goes through `query()` (httr2) for Slides/Sheets, and
  through `googledrive`/`googlesheets4` (httr) for Drive uploads and Sheets.
- `tests/testthat/setup.R` configures auth and vcr:
  - On load it tries `r2slides_auth()` to pick up a **cached gargle token**
    (the common local case). If no token is available it deauths everything
    (`.auth$set_auth_active(FALSE)`, `drive_deauth()`, `gs4_deauth()`) and relies
    entirely on **recorded cassettes**.
  - `vcr::vcr_configure(...)`:
    - `dir = tests/fixtures` — cassettes live here as `.yml`.
    - `filter_request_headers = list(Authorization = "Bearer PLACEHOLDER_AUTH_TOKEN")`
      — the bearer token is scrubbed from recordings.
    - `ignore_hosts = "oauth2.googleapis.com"` — token-refresh calls are NOT
      recorded or matched (so refresh differences never break replay).
    - `record = if CI "none" else "once"` — **locally**, a missing cassette is
      recorded once then replayed; **on CI**, nothing records — every request
      must match an existing cassette or the test errors.

### Why a test can pass locally but fail on CI
Locally you have a token, so live requests succeed (and a missing cassette gets
recorded). On CI there is no token and `record = "none"`, so any request that
doesn't match a recorded interaction throws:

```
Error: Failed to find matching request in active cassette, "<name>".
```

## Recording cassettes (do this interactively)

Cassettes are recorded by a human with a valid Google token, NOT by an agent.
There are "weird interactions" when an agent records, so:

- When you (an agent) add or change a live test, **write the test but do NOT run
  any file containing `vcr::use_cassette(...)`** — with a cached token present,
  `record = "once"` will auto-record. Leave recording to the user.
- Only run pure offline test files and `devtools::load_all()` to verify code.
- Tell the user the exact new cassette name(s) and the command, e.g.
  `devtools::test(filter = "element_class|color")`.
- After the user records, a second run should replay green.

You MAY run a live test file if its cassette already exists — that just replays
(no recording, since `record = "once"` only records when the file is absent).

## The googledrive upload gotcha (image tests)

**Symptom:** a test that calls `add_image()` fails ONLY on CI with
`Failed to find matching request ... add_image(...) -> resolve_image_source ->
upload_to_drive -> googledrive::drive_upload`.

**Cause:** `add_image()` with a local file OR `fit = "fill"` (which downloads the
URL, resizes via `magick`, and writes a local temp file) routes through
`upload_to_drive()` → `googledrive::drive_upload()`. That Drive request carries
auth/session **query parameters** that vary per run. Matching on the full `uri`
therefore fails on CI even though the cassette contains the upload.

**Fix:** match on host + path, not the full uri, in that test's cassette:

```r
vcr::use_cassette(
  "my_image_test",
  match_requests_on = c("method", "host", "path"),  # NOT c("method", "uri")
  { ... }
)
```

`c("method", "host", "path")` ignores the varying query string. This is the
matcher used by the image tests (e.g. `element_get_image_roundtrip` in
`test-element_generics.R`, and `element_class_dispatch_live` in
`test-element_class.R`). Changing the matcher does NOT require re-recording — the
cassette already contains the request; you're only loosening how it's matched.

Notes:
- `magick::image_read(<url>)` (used by both `fit` modes for sizing) fetches over
  its own libcurl, **outside vcr** — a real network call that won't be recorded.
  Prefer URL image sources, and prefer host+path matching for any image test.
- If you need an image element on a slide without any Drive upload or `magick`
  fetch, send a raw `createImage` `batchUpdate` via `query()` with a public URL
  (Google ingests the URL server-side); the only HTTP traffic is the
  deterministic `batchUpdate`.

## General checklist for "Failed to find matching request" on CI

1. Read the backtrace — which call makes the unmatched request?
2. Is it a `googledrive`/`googlesheets4` (httr) request with query params that
   vary per run (uploads, resumable sessions)? → switch that cassette to
   `match_requests_on = c("method", "host", "path")`.
3. Is it an `oauth2.googleapis.com` refresh? → should already be covered by
   `ignore_hosts`; confirm `setup.R` still lists it.
4. Did the test's code path change since the cassette was recorded (new/changed
   requests)? → the cassette is stale; ask the user to delete it and re-record
   interactively.
5. Never "fix" by committing real tokens or by recording on CI.