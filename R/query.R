#' Query Google API's
#'
#' @param endpoint A single string indicating the API endpoint.
#' @param params Optional. A list of parameters for the API request.
#' @param body Optional. A request body.
#' @param base Optional. A string indicating the API base service name. (i.e. 'slides', or 'sheets')
#' @param token Optional. An OAuth2 token. The default uses `r2slides_token()` to find a token.
#' @param call Optional. Call environment used in error messages.
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
  body = NULL,
  base = NULL,
  token = NULL,
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

  if (!rlang::is_empty(req$body) & !is.null(body)) {
    cli::cli_warn(c(
      x = "Internal Error: duplicate body",
      "!" = "A body is supplied both directly and through params.",
      i = "Using `body` and ignoring `params`.",
      i = "Contact the package developers if you ever incounter this error."
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

  if (is_testing() == TRUE) {
    return(req)
  } else {
    rsp <- gargle::request_make(req)
    rsp <- gargle::response_process(rsp)
  }
}


#' Get chart_id from a Google Sheet
#'
#' Retrieves Sheet object from a specified sheet. Errors is 0 or more than 1 chart is found.
#'
#' @param spreadsheet_obj A list of spreadsheet_id (string) and sheet_id (integer).
#' @param token Optional. An OAuth2 token. The default uses `r2slides_token()` to find a token.
#' @param call Optional. Call environment used in error messages.
#'
#' @returns A list of the spreadsheet_id (string) the sheet_id (integer) and chart_id (integer)
#'
#' @export
get_chart_id <- function(
  spreadsheet_obj,
  token = NULL,
  call = rlang::caller_env()
) {
  # Validate inputs
  if (!is.list(spreadsheet_obj) || length(spreadsheet_obj) != 2) {
    cli::cli_abort(
      c(x = "{.arg spreadsheet_obj} must be a list of size 2.")
    )
  }

  if (!rlang::is_string(spreadsheet_obj$spreadsheet_id)) {
    cli::cli_abort(
      c(x = "{.arg spreadsheet_obj$spreadsheet_id} must be a single string.")
    )
  }

  ## Error checking for sheet_id

  # Get spreadsheet data with sheet information
  response <- query2(
    endpoint = 'sheets.spreadsheets.get',
    params = list(
      spreadsheetId = spreadsheet_obj$spreadsheet_id,
      includeGridData = FALSE
    ),
    base = 'sheets',
    token = token
  )

  # Find the matching sheet
  matching_sheet <- purrr::keep(
    response$sheets,
    ~ .x$properties$sheetId == spreadsheet_obj$sheet_id
  )

  if (length(matching_sheet) == 0) {
    cli::cli_abort(
      c(x = "Sheet with ID {sheet_id} not found in spreadsheet.")
    )
  }

  # Check that there is only one chart
  if (length(matching_sheet[[1]]$charts) == 0) {
    cli::cli_abort(
      c(
        x = "Sheet {.var {matching_sheet[[1]]$properties$title}} has no charts."
      )
    )
  } else if (length(matching_sheet[[1]]$charts) > 1) {
    # Create bullet points for each chart found
    chart_bullets <- purrr::map_chr(
      matching_sheet[[1]]$charts,
      ~ {
        chart_type <- .x$spec$basicChart$chartType %||% "Unknown"
        chart_title <- .x$spec$title %||% "Untitled"
        paste0("A ", chart_type, " chart '", chart_title, "' was found")
      }
    )

    cli::cli_abort(
      c(
        x = "Sheet {.var {matching_sheet[[1]]$properties$title}} has more than one chart.",
        i = "{ {length(chart_bullets)} } charts were found:",
        rlang::set_names(chart_bullets, rep("*", length(chart_bullets)))
      )
    )
  } else {
    return(list(
      spreadsheet_id = spreadsheet_obj$spreadsheet_id,
      sheet_id = spreadsheet_obj$sheet_id,
      chart_id = matching_sheet[[1]]$charts[[1]]$chartId
    ))
  }
}
