
#' Writes data to a Google Sheet
#'
#' @param data A data frame to write to the Google Sheet.
#' @param sheet The name of the sheet to write to.
#'
#' @returns A list of spreadsheet_id and sheet_id
#'
#' @export
write_gs <- function(data, sheet) {
  # Check if ss exists in global environment
  if (!exists("ss", envir = .GlobalEnv)) {
    cli::cli_abort(
      "No spreadsheet object 'ss' found in global environment. Initialize with {.code ss <- as_sheets_id(...)}"
    )
  }

  ss <- get("ss", envir = .GlobalEnv)

  # Get current sheet metadata
  ss_metadata <- tryCatch(
    googlesheets4::gs4_get(ss),
    error = function(e) {
      cli::cli_abort("Failed to get spreadsheet metadata:", parent = e)
    }
  )

  # Check if sheet exists
  existing_sheets <- ss_metadata$sheets$name
  sheet_exists <- sheet %in% existing_sheets

  # Create sheet if it doesn't exist
  if (!sheet_exists) {
    cli::cli_alert_info("Sheet '{sheet}' does not exist. Creating it now...")
    tryCatch(
      googlesheets4::sheet_add(ss = ss, sheet = sheet),
      error = function(e) {
        cli::cli_abort("Failed to create sheet '{sheet}':", parent = e)
      }
    )
    cli::cli_alert_success("Sheet '{sheet}' created successfully.")
  }

  # Write data to sheet
  tryCatch(
    googlesheets4::range_write(
      ss = ss,
      data = data,
      sheet = sheet,
      reformat = FALSE
    ),
    error = function(e) {
      cli::cli_abort("Error writing data to sheet '{sheet}':", parent = e)
    }
  )

  # Get sheet_id for the written sheet
  ss_metadata_updated <- googlesheets4::gs4_get(ss)
  sheet_id <- ss_metadata_updated$sheets$id[
    ss_metadata_updated$sheets$name == sheet
  ]

  # Extract spreadsheet ID from ss object
  ss_id <- ss_metadata_updated$spreadsheet_id

  return(list(spreadsheet_id = ss_id, sheet_id = sheet_id))
}
