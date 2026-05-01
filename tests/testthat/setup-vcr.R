library(vcr)

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
