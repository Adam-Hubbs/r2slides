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
