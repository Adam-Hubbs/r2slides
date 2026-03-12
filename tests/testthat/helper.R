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

fake_tokens <- local({
  b64url <- function(x) {
    base64enc::base64encode(charToRaw(x)) |>
      gsub(pattern = "\\+", replacement = "-") |>
      gsub(pattern = "/", replacement = "_") |>
      gsub(pattern = "=+$", replacement = "")
  }

  header <- b64url('{"alg":"RS256","kid":"1","typ":"JWT"}')
  payload <- b64url(
    '{"iss":"https://accounts.google.com","sub":"123456789","email":"test@example.com","email_verified":true,"iat":1700000000,"exp":9999999999}'
  )
  sig <- b64url(strrep("x", 32))

  list(
    access_token = "ya29.A0ARrdaM_vcr_fake_access_token",
    id_token = paste(header, payload, sig, sep = "."),
    refresh_token = "vcr_fake_refresh_token",
    client_id = "vcr_fake_client_id",
    client_secret = "vcr_fake_client_secret"
  )
})

vcr::vcr_configure(
  dir = vcr::vcr_test_path("fixtures"),
  # Scrub the OAuth Bearer token before cassettes are written
  filter_request_headers = list(Authorization = "Bearer <<OAUTH_TOKEN>>"),
  filter_response_headers = list(
    "x-goog-authenticated-user-email" = "<<USER_EMAIL>>"
  ),
  filter_sensitive_data_regex = setNames(
    list(
      '"access_token":\\s*"[^"]+"',
      '"id_token":\\s*"[^"]+"',
      "refresh_token=[^&]+",
      "client_id=[^&]+",
      "client_secret=[^&]+"
    ),
    c(
      sprintf('"access_token": "%s"', fake_tokens$access_token),
      sprintf('"id_token": "%s"', fake_tokens$id_token),
      sprintf("refresh_token=%s", fake_tokens$refresh_token),
      sprintf("client_id=%s", fake_tokens$client_id),
      sprintf("client_secret=%s", fake_tokens$client_secret)
    )
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
