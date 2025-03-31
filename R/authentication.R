#' Get authentication token
#'
#' @description
#' Gets a Oauth Token. If it is not currently set, it called r2slides_auth for authentication.
#'
#' @returns
#' An authentication configuration or `NULL` if authentication is not active.
#'
#' @keywords internal
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
    client = .auth$client %||% r2slides_get_key_from_json(),
    package = "r2slides Testing",
    cache = cache,
    use_oob = use_oob
  )

  print(cred)

  if (!inherits(cred, "Token2.0")) {
    cli::cli_abort(
      "Failed to retrieve a valid token. Please check your authentication setup."
    )
  }

  .auth$set_cred(cred)
  .auth$set_auth_active(TRUE)

  invisible()
}


#' Get an OAuth client from a JSON file
#'
#' @param path Optional. A single string specifying the path to a JSON credentials file.
#' @param name Optional. A single string specifying the client name.
#'
#' @returns
#' A gargle OAuth client.
#'
#' @keywords internal
r2slides_get_key_from_json <- function(path = NULL, name = NULL) {
  # Set Defaults. Only works on Adam's computer (testing purposes)
  # In the future, this file will be bundled with the package and refer to itself internally.
  path <- path %||%
    "~/Y2 Analytics Dropbox/Adam Hubbs/Development/client_secret_573448088645-29u0g68rq8nkuqag9f1ag2cbfqpg8u30.apps.googleusercontent.com.json"
  name <- name %||% "Adam Desktop Client 1"

  gargle::gargle_oauth_client_from_json(
    path = path,
    name = name
  )
}

