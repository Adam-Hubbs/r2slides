# Changelog

## Version 0.0.9075

- New `register_spreadsheet(id)` and `new_spreadsheet(title)` functions
  create a lean R6 `spreadsheet` class that mirrors the `presentation`
  workflow. The active spreadsheet is stored in the package environment
  and retrieved with
  [`get_active_spreadsheet()`](https://adam-hubbs.github.io/r2slides/reference/get_active_spreadsheet.md),
  so
  [`write_gs()`](https://adam-hubbs.github.io/r2slides/reference/write_gs.md)
  no longer requires a global `ss` object.
- [`write_gs()`](https://adam-hubbs.github.io/r2slides/reference/write_gs.md)
  now accepts an
  [`r2slides::spreadsheet`](https://adam-hubbs.github.io/r2slides/reference/spreadsheet.md)
  object, a
  [`googlesheets4::sheets_id`](https://googlesheets4.tidyverse.org/reference/sheets_id.html),
  or any object accepted by
  [`googlesheets4::as_sheets_id()`](https://googlesheets4.tidyverse.org/reference/sheets_id.html)
  via its new `spreadsheet` argument (default:
  [`get_active_spreadsheet()`](https://adam-hubbs.github.io/r2slides/reference/get_active_spreadsheet.md)).
- `sht_id` and `chart_id` are now S7 classes with property access via
  `@`. The old `new_sht_id()` and `new_chart_id()` S3 constructors are
  removed — use
  [`sht_id()`](https://adam-hubbs.github.io/r2slides/reference/sht_id.md)
  and
  [`chart_id()`](https://adam-hubbs.github.io/r2slides/reference/chart_id.md)
  directly.
  [`is_sht_id()`](https://adam-hubbs.github.io/r2slides/reference/sht_id.md)
  now uses S7 inheritance so it returns `TRUE` for `chart_id` objects.
- `create_spreadsheet_env_in_global()` and the `google_spreadsheet`
  global environment pattern are removed.

## Version 0.0.9074

- [`solid_color()`](https://adam-hubbs.github.io/r2slides/reference/r2s_color.md)
  now stores red/green/blue as numeric components (no hex round-trip)
  and accepts all the same inputs as before.
  [`theme_color()`](https://adam-hubbs.github.io/r2slides/reference/r2s_color.md)
  is a new exported class for Google Slides theme color references
  (e.g. `"ACCENT1"`). Both extend a common `r2s_color` base class that
  carries an optional `alpha` channel. The old `transparent_color()`
  class is removed — pass `alpha` directly to
  [`solid_color()`](https://adam-hubbs.github.io/r2slides/reference/r2s_color.md)
  or
  [`theme_color()`](https://adam-hubbs.github.io/r2slides/reference/r2s_color.md)
  instead. A new `visualize()` generic renders a color swatch or theme
  name. Colors with `alpha` set used in text-style contexts now produce
  a warning that alpha will be ignored.

## Version 0.0.9073

- New lazy evaluation strategy: use `set_evaluation_strategy("lazy")` to
  buffer API requests instead of executing them immediately, then flush
  with `execute_requests()`. `get_evaluation_strategy()` returns the
  current setting. `view_request_buffer()` inspects queued requests and
  `clear_request_buffer()` resets the buffer.
  `execute_requests(batch_all = TRUE)` (the default) merges all pending
  `batchUpdate` requests for the same presentation into a single API
  call.

## Version 0.0.9070

- Initial support for tables

## Version 0.0.9064

- Ordering
- Rate-limit fixes
- Bug fixes

## Version 0.0.9055

- Element Class
- Increased test coverage from 7% to 30%ish

## Version 0.0.9050

### New Classes

- Presentation R6 class for handeling presentation state
- Slide object S7 class to represent slides
- peek(slide) shows a thumbnail of the slide in the plots pane

### Refactoring

- Refactored and moved files around - internal facing

## Version 0.0.9030

### Style Objects

- Text style objects can style and text with several different styles
- Style rule objects apply text style objects to text which matches a
  selector function

### Add Text

- Added add_text() and add_text_multi() to add text with relevant
  stylings.

## Version 0.0.9025

### Authentication Fixes

- Fixed Authentication flow and exported more helpers

### Slide Position Objects and Helpers

- New system for Slide Position using S7 objects

### Tools for Linked Charts

- Pipeable syntax for adding linked charts to Google Slides

## Version 0.0.9011

#### Built in default OAuth client

- Built in default OAuth client now means that this package will work
  from anyone’s computer (Once their google account has been
  whitelisted)

## Version 0.0.9010

#### Working version

- Moved everything over to query2 to use the discovery document for
  validation
- Now a working package

## Version 0.0.9000

#### New

- Initial Commit
- Basically just put my local functions in a package. It will not build
  yet. (Namespace stuff mainly)
- CI Workflow setup - test, coverage, and documentation website
  automatically runs

#### Bug fixes

- None
