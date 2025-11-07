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

  if (is.null(ept)) {
    cli::cli_abort(
      c(x = "Endpoint not recognized:", "!" = "{endpoint}"),
      call = call
    )
  }


  # If the endpoint does not contain the base, error
  if (!stringr::str_detect(endpoint, base)) {
    cli::cli_abort(
      c(x = "Incompatible endpoint and base",
        i = "Contact the package developers if you ever incounter this error."),
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

  if(is_testing() == TRUE) {
    return(req)
  } else {
    rsp <- gargle::request_make(req)
    rsp <- gargle::response_process(rsp)
  }
}

mthds


#' Get Chart ID from a Google Sheet
#'
#' Retrieves the chart ID from a specified sheet. Errors if the sheet contains
#' zero charts or more than one chart.
#'
#' @param spreadsheet_id A single string, the ID of the Google Spreadsheet.
#' @param sheet_id A single integer, the ID of the sheet within the spreadsheet.
#' @param token Optional. An OAuth2 token. The default uses `r2slides_token()` to find a token.
#' @param call Optional. Call environment used in error messages.
#'
#' @returns A single integer, the chart ID.
#'
#' @export
get_chart_id <- function(
  spreadsheet_id,
  sheet_id,
  token = NULL,
  call = rlang::caller_env()
) {
  # Validate inputs
  if (!rlang::is_string(spreadsheet_id)) {
    cli::cli_abort(
      c(x = "{.arg spreadsheet_id} must be a single string."),
      call = call
    )
  }

  if (!rlang::is_scalar_integerish(sheet_id)) {
    cli::cli_abort(
      c(x = "{.arg sheet_id} must be a single integer."),
      call = call
    )
  }

  # Get spreadsheet data
  response <- query2(
    endpoint = 'v4.spreadsheets',
    params = list(
      spreadsheetId = spreadsheet_id,
      fields = "sheets(properties.sheetId,charts.chartId)"
    ),
    base = 'sheets',
    token = token,
    call = call
  )

  # Find the matching sheet
  matching_sheet <- purrr::keep(
    response$sheets,
    ~ .x$properties$sheetId == sheet_id
  )

  if (length(matching_sheet) == 0) {
    cli::cli_abort(
      c(x = "Sheet with ID {sheet_id} not found in spreadsheet."),
      call = call
    )
  }

  # Extract charts from the sheet
  charts <- matching_sheet[[1]]$charts

  # Check chart count and return appropriate result
  n_charts <- length(charts)

  if (n_charts == 0) {
    cli::cli_abort(
      c(x = "No charts found on sheet with ID {sheet_id}."),
      call = call
    )
  }

  if (n_charts > 1) {
    cli::cli_abort(
      c(
        x = "Multiple charts found on sheet with ID {sheet_id}.",
        i = "Expected exactly 1 chart, found {n_charts}."
      ),
      call = call
    )
  }

  # Return the single chart ID
  charts[[1]]$chartId
}


maybe_chartid <- get_chart_id(spreadsheetId, sheetID)



### Mthds only has google slides stuff at the moment: Need to digest discovery document
