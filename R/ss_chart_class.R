#' @import S7
NULL

#' Sheet ID class
#'
#' A typed reference to a specific sheet (tab) within a Google Sheets spreadsheet.
#'
#' @param spreadsheet_id A single string. The Google Sheets spreadsheet ID.
#' @param sheet_id A single string. The sheet tab ID.
#'
#' @export
sht_id <- S7::new_class(
  "sht_id",
  properties = list(
    spreadsheet_id = S7::new_property(
      S7::class_character,
      validator = function(value) {
        if (length(value) != 1L) "@spreadsheet_id must be a single string"
      }
    ),
    sheet_id = S7::new_property(
      S7::class_character,
      validator = function(value) {
        if (length(value) != 1L) "@sheet_id must be a single string"
      }
    )
  )
)

#' @rdname sht_id
#' @param x An object to test
#' @export
is_sht_id <- function(x) S7::S7_inherits(x, sht_id)

S7::method(print, sht_id) <- function(x, ...) {
  cli::cli_text("sht_id:")
  cli::cli_bullets(c(
    "Spreadsheet: {.val {x@spreadsheet_id}}",
    "Sheet id: {.val {x@sheet_id}}"
  ))
  invisible(x)
}


#' Chart ID class
#'
#' A typed reference to a specific chart within a Google Sheets sheet. Inherits
#' from [sht_id].
#'
#' @param spreadsheet_id A single string. The Google Sheets spreadsheet ID.
#' @param sheet_id A single string. The sheet tab ID.
#' @param chart_id A single string. The chart ID.
#'
#' @export
chart_id <- S7::new_class(
  "chart_id",
  parent = sht_id,
  properties = list(
    chart_id = S7::new_property(
      S7::class_character,
      validator = function(value) {
        if (length(value) != 1L) "@chart_id must be a single string"
      }
    )
  )
)

S7::method(print, chart_id) <- function(x, ...) {
  cli::cli_text("chart_id:")
  cli::cli_bullets(c(
    "Spreadsheet: {.val {x@spreadsheet_id}}",
    "Sheet id: {.val {x@sheet_id}}",
    "Chart id: {.val {x@chart_id}}"
  ))
  invisible(x)
}


#' Get chart_id from a Google Sheet
#'
#' Retrieves the chart ID from a specified sheet. Errors if 0 or more than 1
#' chart is found.
#'
#' @param spreadsheet_obj A `sht_id` object.
#' @param token Optional. An OAuth2 token. The default uses `r2slides_token()`.
#' @param call Optional. Call environment used in error messages.
#'
#' @returns A `chart_id` object
#'
#' @export
get_chart_id <- function(
  spreadsheet_obj,
  token = NULL,
  call = rlang::caller_env()
) {
  if (!is_sht_id(spreadsheet_obj)) {
    cli::cli_abort(
      c("x" = "{.arg spreadsheet_obj} must be a {.cls sht_id} object.")
    )
  }

  response <- query(
    endpoint = "sheets.spreadsheets.get",
    params = list(
      spreadsheetId = spreadsheet_obj@spreadsheet_id,
      includeGridData = FALSE
    ),
    base = "sheets",
    token = token
  )

  matching_sheet <- purrr::keep(
    response$sheets,
    ~ .x$properties$sheetId == spreadsheet_obj@sheet_id
  )

  if (length(matching_sheet) == 0L) {
    cli::cli_abort(
      c(
        x = "Sheet with ID {.val {spreadsheet_obj@sheet_id}} not found in spreadsheet."
      )
    )
  }

  if (length(matching_sheet[[1]]$charts) == 0L) {
    cli::cli_abort(
      c(
        x = "Sheet {.var {matching_sheet[[1]]$properties$title}} has no charts."
      )
    )
  } else if (length(matching_sheet[[1]]$charts) > 1L) {
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
        i = "{length(chart_bullets)} charts were found:",
        rlang::set_names(chart_bullets, rep("*", length(chart_bullets)))
      )
    )
  }

  chart_id(
    spreadsheet_id = spreadsheet_obj@spreadsheet_id,
    sheet_id = spreadsheet_obj@sheet_id,
    chart_id = as.character(matching_sheet[[1]]$charts[[1]]$chartId)
  )
}
