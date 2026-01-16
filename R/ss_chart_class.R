
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
  response <- query(
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
