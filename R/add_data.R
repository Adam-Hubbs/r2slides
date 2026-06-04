#' Write data to a Google Sheet
#'
#' `write_gs()` takes data and writes it to a Sheet. It is a wrapper around `googlesheets4::range_write()`.
#'
#' @param data A data frame to write to the Google Sheet.
#' @param sheet The name of the sheet to write to.
#' @param spreadsheet Optional. An `r2slides::spreadsheet` object, a
#'   [googlesheets4::sheets_id], or any object accepted by
#'   [googlesheets4::as_sheets_id()] (spreadsheet ID string or URL). Defaults
#'   to [get_active_spreadsheet()].
#'
#' @returns A `sht_id` object.
#' @examples
#' if(FALSE){
#'   mtcars |>
#'     write_gs("Cars")
#' }
#' @export
write_gs <- function(data, sheet, spreadsheet = get_active_spreadsheet()) {
  ss_id <- tryCatch(
    as.character(googlesheets4::as_sheets_id(spreadsheet)),
    error = \(e)
      cli::cli_abort(
        c(
          "{.arg spreadsheet} must be an {.cls r2slides::spreadsheet} or a googlesheets4-compatible object.",
          "i" = "Pass a {.cls sheets_id}, a spreadsheet URL, or a spreadsheet ID string."
        ),
        parent = e
      )
  )

  metadata <- query(
    endpoint = "sheets.spreadsheets.get",
    params = list(spreadsheetId = ss_id, includeGridData = FALSE),
    base = "sheets"
  )

  existing_sheets <- purrr::map_chr(metadata$sheets, ~ .x$properties$title)

  if (!sheet %in% existing_sheets) {
    cli::cli_alert_info(
      "Sheet {.val {sheet}} does not exist. Creating it now..."
    )
    tryCatch(
      googlesheets4::sheet_add(ss = ss_id, sheet = sheet),
      error = \(e)
        cli::cli_abort("Failed to create sheet {.val {sheet}}.", parent = e)
    )
    cli::cli_alert_success("Sheet {.val {sheet}} created successfully.")
  }

  tryCatch(
    googlesheets4::range_write(
      ss = ss_id,
      data = data,
      sheet = sheet,
      reformat = FALSE
    ),
    error = \(e)
      cli::cli_abort("Error writing data to sheet {.val {sheet}}.", parent = e)
  )

  metadata_updated <- query(
    endpoint = "sheets.spreadsheets.get",
    params = list(spreadsheetId = ss_id, includeGridData = FALSE),
    base = "sheets"
  )

  sheet_props <- purrr::detect(
    metadata_updated$sheets,
    ~ .x$properties$title == sheet
  )

  sht_id(
    spreadsheet_id = metadata_updated$spreadsheetId,
    sheet_id = as.character(sheet_props$properties$sheetId)
  )
}
