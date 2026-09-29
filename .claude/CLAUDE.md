# CLAUDE.md

This file provides guidance to Agents working with code in this repository.

## Package Overview

`r2slides` is an R package for programmatically creating and modifying Google Slides presentations and linking them to Google Sheets data. It wraps the Google Slides/Sheets REST APIs behind a fluent, pipeable R interface.

## Common Commands

```r
# Install dependencies and load for development
devtools::load_all()

# Run all tests
devtools::test()

# Run a single test file
testthat::test_file("tests/testthat/test-slide_position.R")

# Run R CMD CHECK
devtools::check()

# Update documentation
devtools::document()
```

## Architecture

### Three-Layer Design

**1. API Layer (`query.R`)**
All Google API calls go through `query()`, which handles OAuth via `gargle`, HTTP via `httr2`, and automatic exponential backoff on 429/503 errors. Internal endpoint maps `mthds_slides` and `mthds_sheets` (in `Internals.R`) define all API methods.

**2. Object Layer (S7 + R6 classes)**
- **`presentation`** (R6, `new_presentation.R`) — Stateful object. Holds the active presentation ID, a slide cache, and a ledger of created elements (element_id, slide_id, type, timestamps, deleted flag). The active presentation is stored in a package-level `.r2slides_objects` environment and accessed via `get_active_presentation()`.
- **`slide`** (S7, `new_slide.R`) — Individual slide with element list.
- **`element` / `text_element`** (S7, `element_class.R`) — Individual content items on a slide.
- **`slide_position`** (S7, `slide_position.R`) — Immutable coordinate value object. Stores dimensions in EMU (914,400 EMU = 1 inch). Rotation is stored in degrees and converted to affine transform components (scaleX, scaleY, shearX, shearY) as computed properties for the API.
- **`r2slides_table`** (S7, `tables.R`) — Complex table with cell-level border and styling.
- **`sht_id` / `chart_id`** (S7, `ss_chart_class.R`) — Typed references to Google Sheets sheets and charts.

**3. User-Facing API**
High-level functions compose the layers into a fluent workflow:
```r
chart_data |>
  write_gs("Sheet Name") |>
  get_chart_id() |>
  add_linked_chart(on_slide_number(4), in_top_left())
```

### Key Design Patterns

- **Active presentation global state** — `new_presentation()` / `register_presentation()` set the active presentation; all `add_*` functions operate on it by default.
- **Slide selectors** — `on_slide_number()`, `on_slide_id()`, `on_slide_url()`, `on_slide_after()`, `on_slide_with_notes()` return slide selection objects consumed by `add_*` functions (`slide_selection.R`).
- **Preset positions** — `in_top_left()`, `in_top_middle()`, `in_top_right()`, `in_bottom_*()` return `slide_position` objects (`slide_position.R`).
- **Argument recycling in `add_text_multi()`** — scalar/vector arguments are recycled to a common length determined by the longest vector (`get_safe_length()` in `utils.R`).
- **Style rules** (`style_text.R`) — `style_rule()` supports `default`, `match_text`, `regex`, and `func` strategies for conditional text styling; `combine_style()` merges multiple `text_style` objects.

### Coordinate System

All positions passed to the Google API must be in EMU. `correct_slide_size()` handles conversion from PowerPoint dimensions (which differ from Google Slides defaults). Rotation uses affine transform math computed in `slide_position.R`.

## Testing

Tests use `testthat` (edition 3) with `vcr` for HTTP cassette recording (fixtures in `tests/fixtures/`) and `vdiffr` for visual diffs. Tests touching the Google API use recorded cassettes and do not make live requests. Authentication is not required to run the test suite.

See [.claude/authentication.md](authentication.md) for how auth/vcr work in tests, the interactive cassette-recording workflow, and how to fix "Failed to find matching request" CI failures (notably the googledrive upload matcher gotcha).

## Package Conventions

- S7 is used for immutable value objects; R6 is used for stateful objects with mutable fields.
- Internal API endpoint definitions live in `Internals.R` (`mthds_slides`, `mthds_sheets`).
- `aaa.R` and `onload.R` handle package-level setup and run before all other files.
- `zzz-peek.R` ensures `peek()` loads last (file naming convention).