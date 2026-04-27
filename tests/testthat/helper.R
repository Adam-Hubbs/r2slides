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

test_token <- list("Test Token")


vcr::vcr_configure(
  dir = vcr::vcr_test_path("fixtures"),
  # Scrub the OAuth Bearer token before cassettes are written
  filter_request_headers = list(Authorization = "Bearer <<OAUTH_TOKEN>>"),
  filter_response_headers = list(
    "x-goog-authenticated-user-email" = "<<USER_EMAIL>>"
  ),
  filter_sensitive_data_regex = list(
    '"access_token": "<<access_token>>"' = '"access_token":\\s*"[^"]+"',
    '"id_token": "<<id_token>>"' = '"id_token":\\s*"[^"]+"',
    "refresh_token=<<refresh_token>>" = "refresh_token=[^&]+",
    "client_id=<<client_id>>" = "client_id=[^&]+",
    "client_secret=<<client_secret>>" = "client_secret=[^&]+"
  )
)

# Snapshot transformer — replaces the last_refreshed timestamp printed by
# presentation$print() so snapshots are not invalidated by the wall-clock value.
# Format is set in presentation$print(): "%Y-%m-%d %H:%M:%S"
scrub_last_refreshed <- function(lines) {
  gsub(
    pattern = "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}",
    replacement = "<<Last Refreshed Value>>",
    x = lines
  )
}

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

# Flextable with explicit top/bottom border colours
make_bordered_ft <- function() {
  flextable::flextable(dplyr::tibble(x = c("A", "B"), y = 1:2)) |>
    flextable::border(
      i = 1,
      border.top = officer::fp_border(color = "#FF0000", width = 2),
      part = "body"
    ) |>
    flextable::border(
      i = 2,
      border.bottom = officer::fp_border(color = "#0000FF", width = 1, style = "dashed"),
      part = "body"
    )
}

# A fixed slide_position used across table request tests
test_table_position <- function() {
  slide_position(top = 1, left = 1, width = 8, height = 5)
}

# Pull the first request of a given type out of a named request list
first_req <- function(reqs, type) {
  found <- Filter(\(r) !is.null(r[[type]]), reqs)
  if (length(found) == 0L) return(NULL)
  found[[1L]][[type]]
}

# Find a border request matching row_index / col_index / borderPosition
find_border_req <- function(border_reqs, row_idx, col_idx, position) {
  Filter(
    \(r) {
      u <- r$updateTableBorderProperties
      !is.null(u) &&
        u$tableRange$location$rowIndex    == row_idx &&
        u$tableRange$location$columnIndex == col_idx &&
        u$borderPosition == position
    },
    border_reqs
  )
}
