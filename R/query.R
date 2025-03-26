#' Query Google API's
#'
#' @param endpoint A single string indicating the API endpoint.
#' @param params Optional. A list of parameters for the API request.
#' @param call Optional. Call environment used in error messages.
#' @param token Optional. An OAuth2 token. The default uses `r2slides_token()` to find a token.
#' @param body Optional. A request body.
#' @param base Optional. A string indicating the API base service name. (i.e. 'slides', or 'sheets')
#' @param ... Additional arguments reserved for future expansion.
#'
#' @returns
#' A processed API response. Will error if the endpoint is not recognized
#' or if required parameters are missing or malformed.
#'
#' @export
query2 <- function(
    endpoint = 'slides.presentations.batchUpdate',
    params = list(),
    call = caller_env(),
    token = NULL,
    body = NULL,
    base = NULL,
    ...
) {
  #Base
  base <- base %||% 'slides'
  base_url <- str_c('https://', base, '.googleapis.com')

  ept <- mthds[[endpoint]]

  if (is.null(ept)) {
    cli::cli_abort(
      c(x = "Endpoint not recognized:", "!" = "{endpoint}"),
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

  if (!is_empty(req$body) & !is.null(body)) {
    cli::cli_warn(c(
      x = "Duplicate body",
      "!" = "A body is supplied both directly and through params.",
      i = "Using `body` and ignoring `params`."
    ))
  }

  body <- body %||% req$body

  req <- gargle::request_build(
    path = req$path,
    method = req$method,
    params = req$params,
    body = body,
    base_url = req$base_url,
    token = token %||% r2slides_token()
  )

  if(is_testing() == FALSE) {
    return(req)
  } else {
    rsp <- request_make(req)
    rsp <- response_process(rsp)
  }
}















