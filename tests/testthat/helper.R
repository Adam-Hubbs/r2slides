default_title_args <- list(
  title_font_size = NULL,
  title_font_family = NULL,
  title_font_bold = NULL,
  title_color = NULL,
  title_bg_color = NULL,
  title_left = NULL,
  title_top = NULL,
  title_width = NULL,
  title_height = NULL
)

default_commentary_args <- list(
  commentary_font_family = NULL,
  commentary_color = NULL,
  commentary_bg_color = NULL,
  commentary_left = NULL,
  commentary_top = NULL,
  commentary_width = NULL,
  commentary_height = NULL
)

default_footer_args <- list(
  footer_font_size = NULL,
  footer_font_family = NULL,
  footer_color = NULL,
  footer_left = NULL,
  footer_top = NULL,
  footer_width = NULL,
  footer_height = NULL
)

# ── Table test helpers ──────────────────────────────────────────────────────

# A minimal plain flextable: 3 body rows x 3 columns, no merges/borders
make_plain_ft <- function() {
  dplyr::tibble(a = c("r1", "r2", "r3"), b = 1:3, c = c(TRUE, FALSE, TRUE)) |>
    flextable::flextable()
}

# Flextable with a vertical merge spanning rows 1:2 in column 1 (body)
make_merged_ft <- function() {
  flextable::flextable(dplyr::tibble(x = c("A", "A", "B"), y = 1:3)) |>
    flextable::merge_at(i = 1:2, j = 1)
}

# Flextable with explicit top/bottom border colours.
# Layout: 1 header row + 5 body rows x 2 cols (row_index 0..5).
#   - Header (row_index 0): thick red (3pt) top border
#   - Body row 5 (last, row_index 5): visible blue (1.5pt) bottom border
make_bordered_ft <- function() {
  flextable::flextable(
    dplyr::tibble(x = c("A", "B", "C", "D", "E"), y = 1:5)
  ) |>
    flextable::border(
      border.top = officer::fp_border(color = "#FF0000", width = 3),
      part = "header"
    ) |>
    flextable::border(
      i = 5,
      border.bottom = officer::fp_border(color = "#0000FF", width = 1.5),
      part = "body"
    )
}

# Flextable with a variety of borders and merged cells (uses built-in airquality).
# Layout:
#   - 5 body rows x 4 cols: Ozone, Solar.R, Wind, Temp
#   - Merge: body rows 1:2 col 1 (vertical)
#   - Merge: body rows 4:5 cols 3:4 (2x2 block)
#   - Header: thick red (3pt) top border
#   - Body row 3: dashed blue (1pt) bottom border
#   - Body col 1: green (2pt) left border
#   - Body col 4: white (1pt) right border  — should be invisible
#   - Body row 5: explicit zero-width top border — should suppress Google Slides default
make_borders_ft <- function() {
  df <- airquality[1:5, c("Ozone", "Solar.R", "Wind", "Temp")]

  flextable::flextable(df) |>
    # Vertical merge: rows 1:2 in col 1
    flextable::merge_at(i = 1:2, j = 1, part = "body") |>
    # 2x2 block merge: rows 4:5, cols 3:4
    flextable::merge_at(i = 4:5, j = 3:4, part = "body") |>
    # Thick red top border on entire header
    flextable::border(
      border.top = officer::fp_border(color = "#FF0000", width = 3),
      part = "header"
    ) |>
    # Dashed blue bottom border on body row 3
    flextable::border(
      i = 3,
      border.bottom = officer::fp_border(
        color = "#0000FF",
        width = 1,
        style = "dashed"
      ),
      part = "body"
    ) |>
    # Green left border on all body cells in col 1
    flextable::border(
      j = 1,
      border.left = officer::fp_border(color = "#00AA00", width = 2),
      part = "body"
    ) |>
    # White (invisible) right border on col 4 body
    flextable::border(
      j = 4,
      border.right = officer::fp_border(color = "#FFFFFF", width = 1),
      part = "body"
    ) |>
    # Explicit zero-width (no border) on top of body row 5
    flextable::border(
      i = 5,
      border.top = officer::fp_border(color = "#000000", width = 0),
      part = "body"
    )
}

# A fixed slide_position used across table request tests
test_table_position <- function() {
  slide_position(top = 1, left = 1, width = 8, height = 5)
}

# ── vcr / API test helpers ────────────────────────────────────────────────────

# Passed as `token` in tests that exercise query() error paths — those tests
# abort before any HTTP call, so the token value is irrelevant.
test_token <- NULL

# IDs for the dedicated test Google Slides / Sheets documents.
TEST_PRESENTATION_ID <- Sys.getenv(
  "R2SLIDES_TEST_PRESENTATION_ID",
  "1K9z9yY8Z9qmzOvY-qmO_eNYSpstJvBRObQdnsweaXnY"
)
TEST_SPREADSHEET_ID <- Sys.getenv("R2SLIDES_TEST_SPREADSHEET_ID", "1AUEefHOPD26yefuBsxdyL5vw2DmopSxH308WuttmFCI")

# Sends a deleteObject batchUpdate for a slide. Used for state cleanup inside
# vcr cassette blocks so every test that creates a slide also removes it.
delete_slide_raw <- function(ps, slide_id) {
  query(
    endpoint = "slides.presentations.batchUpdate",
    params = list(presentationId = ps$presentation_id),
    body = list(
      requests = list(list(deleteObject = list(objectId = slide_id)))
    ),
    base = "slides"
  )
  invisible(NULL)
}

# Pull the first request of a given type out of a named request list
first_req <- function(reqs, type) {
  found <- Filter(\(r) !is.null(r[[type]]), reqs)
  if (length(found) == 0L) {
    return(NULL)
  }
  found[[1L]][[type]]
}

# Find a border request matching row_index / col_index / borderPosition
find_border_req <- function(border_reqs, row_idx, col_idx, position) {
  Filter(
    \(r) {
      u <- r$updateTableBorderProperties
      !is.null(u) &&
        u$tableRange$location$rowIndex == row_idx &&
        u$tableRange$location$columnIndex == col_idx &&
        u$borderPosition == position
    },
    border_reqs
  )
}


vcr::vcr_configure(
  dir = testthat::test_path("../fixtures"),
  filter_request_headers = list(
    Authorization = "Bearer PLACEHOLDER_AUTH_TOKEN"
  ),
  ignore_hosts = "oauth2.googleapis.com",
  record = if (identical(Sys.getenv("CI"), "true")) "none" else "once"
)

# If .auth$cred is already a Token2.0 (e.g. developer called r2slides_auth()
# in this session), nothing to do. Otherwise, try to load a cached gargle
# token from disk. If that succeeds (local dev with prior auth), set it and
# keep auth active so vcr can record real API responses. If it fails (CI or
# no gargle cache), deactivate all auth so tests replay from cassettes.
if (!inherits(r2slides:::.auth$cred, "Token2.0")) {
  cred <- tryCatch(
    gargle::token_fetch(
      scopes = c(
        "https://www.googleapis.com/auth/spreadsheets",
        "https://www.googleapis.com/auth/presentations",
        "https://www.googleapis.com/auth/drive"
      ),
      client = r2slides:::.auth$client %||%
        r2slides:::r2slides_default_client(),
      package = "r2slides"
    ),
    error = function(e) NULL
  )

  if (inherits(cred, "Token2.0")) {
    r2slides:::.auth$set_cred(cred)
    r2slides:::.auth$set_auth_active(TRUE)
  } else {
    r2slides:::.auth$set_auth_active(FALSE)
    googledrive::drive_deauth()
    googlesheets4::gs4_deauth()
    withr::defer(
      r2slides:::.auth$set_auth_active(TRUE),
      envir = testthat::teardown_env()
    )
  }
}
