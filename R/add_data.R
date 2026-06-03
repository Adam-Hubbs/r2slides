#' Writes data to a Google Sheet
#'
#' @param data A data frame to write to the Google Sheet.
#' @param sheet The name of the sheet to write to.
#' @param spreadsheet Optional. An `r2slides::spreadsheet` object, a
#'   [googlesheets4::sheets_id], or any object accepted by
#'   [googlesheets4::as_sheets_id()] (spreadsheet ID string or URL). Defaults
#'   to [get_active_spreadsheet()].
#'
#' @returns A `sht_id` object
#'
#' @export
write_gs <- function(data, sheet, spreadsheet = get_active_spreadsheet()) {
  ss <- tryCatch(
    googlesheets4::as_sheets_id(spreadsheet),
    error = \(e)
      cli::cli_abort(
        c(
          "{.arg spreadsheet} must be an {.cls r2slides::spreadsheet} or a googlesheets4-compatible object.",
          "i" = "Pass a {.cls sheets_id}, a spreadsheet URL, or a spreadsheet ID string."
        ),
        parent = e
      )
  )

  ss_metadata <- tryCatch(
    googlesheets4::gs4_get(ss),
    error = \(e)
      cli::cli_abort("Failed to get spreadsheet metadata.", parent = e)
  )

  if (!sheet %in% ss_metadata$sheets$name) {
    cli::cli_alert_info(
      "Sheet {.val {sheet}} does not exist. Creating it now..."
    )
    tryCatch(
      googlesheets4::sheet_add(ss = ss, sheet = sheet),
      error = \(e)
        cli::cli_abort("Failed to create sheet {.val {sheet}}.", parent = e)
    )
    cli::cli_alert_success("Sheet {.val {sheet}} created successfully.")
  }

  tryCatch(
    googlesheets4::range_write(
      ss = ss,
      data = data,
      sheet = sheet,
      reformat = FALSE
    ),
    error = \(e)
      cli::cli_abort("Error writing data to sheet {.val {sheet}}.", parent = e)
  )

  ss_metadata_updated <- googlesheets4::gs4_get(ss)
  sheet_id <- as.character(
    ss_metadata_updated$sheets$id[ss_metadata_updated$sheets$name == sheet]
  )

  sht_id(
    spreadsheet_id = ss_metadata_updated$spreadsheet_id,
    sheet_id = sheet_id
  )
}
