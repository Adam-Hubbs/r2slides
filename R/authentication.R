#' Get authentication token
#'
#' @description
#' Gets a Oauth Token. If it is not currently set, it called r2slides_auth for authentication.
#'
#' @returns
#' An authentication configuration or `NULL` if authentication is not active.
r2slides_token <- function() {
  if (isFALSE(.auth$auth_active)) {
    return(NULL)
  }
  if (!inherits(.auth$cred, "Token2.0")) {
    r2slides_auth()
  }
  httr::config(token = .auth$cred)
}


#' Create OAuth authentication token
#'
#' @param email Optional. A string giving an email address that can be used to identify the user.
#' @param scopes Optional. A character vector of requested OAuth scopes.
#' @param cache Optional. Specifies the OAuth token cache.
#' @param use_oob Optional. Whether to use out-of-band authentication.
#'
#' @returns
#' Nothing directly. Updates the .auth token. Will error if unable to retrieve a valid
#' OAuth2.0 token.
#'
#' @keywords internal
r2slides_auth <- function(
    email = gargle::gargle_oauth_email(),
    scopes = c(
      "https://www.googleapis.com/auth/spreadsheets",
      "https://www.googleapis.com/auth/presentations",
      "https://www.googleapis.com/auth/drive"
    ),
    cache = gargle::gargle_oauth_cache(),
    use_oob = gargle::gargle_oob_default()
) {
  # this catches a common error, where the user passes JSON for an OAuth client
  # to the `path` argument, which only expects a service account token
  #TODO: Change this error to point to r2slides package
  gargle::check_is_service_account(NULL, hint = "drive_auth_configure")

  cred <- gargle::token_fetch(
    scopes = scopes,
    client = .auth$client %||% r2slides_default_client(),
    package = "r2slides",
    cache = cache,
    use_oob = use_oob
  )

  if (!inherits(cred, "Token2.0")) {
    cli::cli_abort(
      "Failed to retrieve a valid token. Please check your authentication setup."
    )
  }

  .auth$set_cred(cred)
  .auth$set_auth_active(TRUE)


  # Set auth in gs4 and googlesdrive
  googledrive::drive_auth(token = r2slides_token())
  googlesheets4::gs4_auth(token = r2slides_token())

  invisible()
}

    


#' Get the default OAuth client
#'
#' @returns
#' A gargle OAuth client.
#'
#' @keywords internal
r2slides_default_client <- function(path = NULL, name = NULL) {
  path <- path %||% "inst/encrypted_json.json" # Figure out various States for this, and make it work everywhere
  name <- name %||% "r2slides y2 client"


  decrypted_json <- gargle::secret_decrypt_json(path, R2SLIDES_KEY)

  gargle::gargle_oauth_client_from_json(
    path = decrypted_json,
    name = name
  )
}


#' Clear current token
#'
#' If you ever get an error saying client error: (401) UNAUTHENTICATED with a warning message of `Unable to refresh token: invalid_grant`,
#' then run this function, and restart the R Session. It should restart the auth process next time you need it.
#'
#' @export
r2slides_deauth <- function() {
  .auth$clear_cred()
  invisible()
}

