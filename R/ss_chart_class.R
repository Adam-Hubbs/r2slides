#' Sheet Id class
#'
#' @param gs4_sheet A googlesheets4 sheet id, or something coercible to one
#' @param sheet_id An integer referencing a sheet id
#' @param call Optional. Call environment used in error messages.
#' @param x An object of class `sht_id`
#' @param ... Additional arguments from print
#'
#' @rdname new_sht_id
#' @export
new_sht_id <- function(gs4_sheet, sheet_id, call = rlang::caller_env()) {
  gs4_obj <- tryCatch(
    googlesheets4::as_sheets_id(gs4_sheet),
    error = function(e) {
      cli::cli_abort(
        c(
          "x" = "{.arg gs4_sheet} must be coercible to a googlesheets4 sheets id."
        ),
        call = call
      )
    }
  )

  if (!rlang::is_string(sheet_id)) {
    cli::cli_abort(
      c("x" = "{.arg sheet_id} must be a single string."),
      call = call
    )
  }

  structure(
    list(
      gs4_sheet = gs4_sheet,
      sheet_id = sheet_id
    ),
    class = "sht_id"
  )
}

#' @rdname new_sht_id
#' @export
is_sht_id <- function(x) {
  inherits(x, "sht_id") &&
    is.list(x) &&
    !is.null(x$gs4_sheet) &&
    rlang::is_string(x$sheet_id)
}

#' @rdname new_sht_id
#' @export
print.sht_id <- function(x, ...) {
  cli::cli_text("sht_id:")
  cli::cli_bullets(c(
    "Spreadsheet: {.val {as.character(x$gs4_sheet)}}",
    "Sheet id: {.val {x$sheet_id}}"
  ))
  invisible(x)
}


#' Chart Id class
#'
#' @param gs4_sheet A googlesheets4 sheet id, or something coercible to one
#' @param sheet_id An integer referencing a sheet id
#' @param chart_id An integer referencing a chart id
#' @param call Optional. Call environment used in error messages.
#' @param x An object of class `chart_id`
#' @param ... Additional arguments from print
#'
#' @rdname new_chart_id
#' @export
new_chart_id <- function(
  gs4_sheet,
  sheet_id,
  chart_id,
  call = rlang::caller_env()
) {
  gs4_obj <- tryCatch(
    googlesheets4::as_sheets_id(gs4_sheet),
    error = function(e) {
      cli::cli_abort(
        c(
          "x" = "{.arg gs4_sheet} must be coercible to a googlesheets4 sheets id."
        ),
        call = call
      )
    }
  )

  if (!rlang::is_string(sheet_id)) {
    cli::cli_abort(
      c("x" = "{.arg sheet_id} must be a single string."),
      call = call
    )
  }

  if (!rlang::is_scalar_integer(chart_id)) {
    cli::cli_abort(
      c("x" = "{.arg chart_id} must be a single integer"),
      call = call
    )
  }

  structure(
    list(
      gs4_sheet = gs4_sheet,
      sheet_id = sheet_id,
      chart_id = chart_id
    ),
    class = c("chart_id", "sht_id")
  )
}

#' @rdname new_chart_id
#' @export
print.chart_id <- function(x, ...) {
  cli::cli_text("chart_id:")
  cli::cli_bullets(c(
    "Spreadsheet: {.val {as.character(x$gs4_sheet)}}",
    "Sheet id: {.val {x$sheet_id}}",
    "Chart id: {.val {x$chart_id}}"
  ))
  invisible(x)
}


#' Get chart_id from a Google Sheet
#'
#' Retrieves Sheet object from a specified sheet. Errors is 0 or more than 1 chart is found.
#'
#' @param spreadsheet_obj A sht_id object.
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
  if (!is_sht_id(spreadsheet_obj)) {
    cli::cli_abort(
      c(
        'x' = "{.arg spreadsheet_obj} must be a {.cls sht_id} object."
      )
    )
  }

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
    return(new_chart_id(
      gs4_sheet = spreadsheet_obj$spreadsheet_id,
      sheet_id = spreadsheet_obj$sheet_id,
      chart_id = matching_sheet[[1]]$charts[[1]]$chartId
    ))
  }
}
