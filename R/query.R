#' Query Google API's
#'
#' @param endpoint A single string indicating the API endpoint.
#' @param params Optional. A list of parameters for the API request.
#' @param body Optional. A request body.
#' @param base Optional. A string indicating the API base service name. (i.e. 'slides', or 'sheets')
#' @param token Optional. An OAuth2 token. The default uses `r2slides_token()` to find a token.
#' @param debug Optional. If `TRUE`, return the unexecuted request. If `FALSE`, execute the request.` Default: `FALSE`.
#' @param call Optional. Call environment used in error messages.
#' @param ... Additional arguments reserved for future expansion.
#'
#' @returns
#' A processed API response. Will error if the endpoint is not recognized
#' or if required parameters are missing or malformed.
#'
#' @export
query <- function(
  endpoint = 'slides.presentations.batchUpdate',
  params = list(),
  body = NULL,
  base = NULL,
  token = NULL,
  debug = FALSE,
  call = rlang::caller_env(),
  ...
) {
  #Base
  base <- base %||% 'slides'
  #Check that this pattern also works for sheets
  base_url <- stringr::str_c('https://', base, '.googleapis.com')

  if (base == 'slides') {
    ept <- mthds_slides[[endpoint]]
  } else if (base == 'sheets') {
    ept <- mthds_sheets[[endpoint]]
  } else {
    cli::cli_abort(
      c(x = "Base not recognized:", "!" = "{base}"),
      call = call
    )
  }

  # If the endpoint does not contain the base, error
  if (!stringr::str_detect(endpoint, base)) {
    cli::cli_abort(
      c(
        x = "Endpoint not recognized:",
        "!" = "{endpoint}",
        i = "Endpoint must be valid according to the relevant discovery document.",
        i = "Check that the base matches the requested endpoint.",
        i = "Contact the package developers if you ever incounter this error."
      ),
      call = call
    )
  }

  req <- rlang::try_fetch(
    {
      gargle::request_develop(
        endpoint = ept,
        params = params,
        base_url = base_url
      )
    },
    error = function(e) {
      cli::cli_abort(
        c(x = "Missing or malformed argument:"),
        call = call,
        parent = e
      )
    }
  )

  if (!rlang::is_empty(req$body) && !is.null(body)) {
    cli::cli_warn(
      c(
        x = "Internal Error: duplicate body",
        "!" = "A body is supplied both directly and through params.",
        i = "Using `body` and ignoring `params`.",
        i = "Contact the package developers if you ever incounter this error."
      ),
      call = call
    )
  }

  body <- body %||% req$body

  # Special handeling for "GET" methods until this github issue is fixed: https://github.com/r-lib/gargle/issues/292
  if (req$method == 'GET') {
    req <- gargle::request_build(
      path = add_path_params(
        req,
        "presentationId",
        "pageObjectId",
        "spreadsheetId"
      ),
      method = req$method,
      params = NULL,
      body = body,
      base_url = req$base_url,
      token = token %||% r2slides_token()
    )
  } else {
    req <- gargle::request_build(
      path = req$path,
      method = req$method,
      params = req$params,
      body = body,
      base_url = req$base_url,
      token = token %||% r2slides_token()
    )
  }

  if (is_testing() || debug) {
    return(req)
  } else {
    rsp <- gargle::request_make(req)
    rsp <- gargle::response_process(rsp, call = call)
  }
}


# Fixes gargle's path bugs for GET methods
add_path_params <- function(req, ...) {
  args <- rlang::list2(...)

  # If no args specified, return path unchanged
  if (length(args) == 0) {
    return(req$path)
  }

  # Start with the original path
  path <- req$path

  # Replace each specified parameter in the path
  for (arg in args) {
    if (!is.null(req$params[[arg]]) && !is.na(req$params[[arg]])) {
      pattern <- stringr::str_glue("{{{arg}}}") |> as.character()
      plus_pattern <- stringr::str_glue("{{+{arg}}}") |> as.character()

      path <- stringr::str_replace(
        path,
        stringr::fixed(pattern),
        req$params[[arg]]
      ) |>
        stringr::str_replace(
          stringr::fixed(plus_pattern),
          req$params[[arg]]
        )
    }
  }

  return(path)
}
