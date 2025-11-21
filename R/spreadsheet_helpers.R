#' Validate spreadsheet environment
#'
#' @param spreadsheet A single string naming a spreadsheet environment.
#'
#' @returns
#' Nothing. The function will error if the specified environment doesn't
#' exist in `.GlobalEnv` or if it's missing any of the required objects
#' (`spreadsheet_id`, `sheet_ids`).
#'
#' @keywords internal
validatespreadsheet <- function(spreadsheet) {
  spreadsheet <- rlang::sym(spreadsheet)

  if (!exists(spreadsheet, envir = .GlobalEnv)) {
    cli::cli_abort(
      c(
        "Invalid or missing spreadsheet.",
        i = "Provide a valid environment.",
        i = "You can create a new spreadsheet or register a spreadsheet to fix this problem."
      ),
      call = rlang::caller_env()
    )
  }

  # Check for required objects
  required_objects <- c("spreadsheet_id", "sheet_ids")
  missing_objects <- setdiff(required_objects, ls(get(spreadsheet)))

  if (length(missing_objects) > 0) {
    cli::cli_abort(
      c(
        "Your spreadsheet is corrupted or missing.",
        x = "The environment {.var {spreadsheet}} is missing required objects:",
        rlang::set_names(missing_objects, rep("i", length(missing_objects)))
      ),
      call = rlang::caller_env()
    )
  }

  # Future spreadsheet validation checks go here
}


#' Create spreadsheet environment
#'
#' @returns
#' Creates a `google_spreadsheet` environment in the global environment if one does
#' not already exist.
#'
#' @export
create_spreadsheet_env_in_global <- function() {
  if (!exists("google_spreadsheet", envir = .GlobalEnv)) {
    eval(
      quote(.GlobalEnv$google_spreadsheet <- new.env(parent = emptyenv())),
      envir = .GlobalEnv
    )
  }
}


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
