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
