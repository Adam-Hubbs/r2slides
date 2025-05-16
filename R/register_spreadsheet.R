

register_spreadsheet <- function(
  name = NULL,
  url = NULL,
  spreadsheet_id = NULL,
  local = FALSE,
  ...
) {
#Registers a spreadsheet
#Name or spreadsheet_id of a slides spreadsheet in your google drive
#Registers that spreadsheet in R
#Can register the spreadsheet to the global env or a new env (to have the ability to edit multiple spreadsheets at the same time)

# Check if more than one identifier is provided
identifiers_provided <- sum(
  !is.null(name),
  !is.null(url),
  !is.null(spreadsheet_id)
)
if (identifiers_provided > 1) {
  cli::cli_abort(
    "Please provide only one of {.var name}, {.var url}, or {.var spreadsheet_id}"
  )
} else if (identifiers_provided == 0) {
  # No identifier provided
  cli::cli_abort(
    "Please provide one of {.var name}, {.var url}, or {.var spreadsheet_id}"
  )
}

# Error handling flow when no files found by drive needs updating. Currently drive puts out warnings which get printed then we error later.
# Need to catch the googledrive warnings, wrap our error around, then print googledrive errors (after turning them into errors).

# Also need to improve error handling when multiple files have the same name and match the name argument of register_spreadsheet.
# Currently gives warnings and then the readLines for overriding. Do not override unless we have a match.


# Handle spreadsheet_id
if (!is.null(spreadsheet_id)) {
  if (!is.character(spreadsheet_id)) {
    cli::cli_abort("{.var spreadsheet_id} must be a {.code character}")
  } else {
    r <- tryCatch(
      googledrive::drive_get(id = spreadsheet_id),
      error = \(e)
      cli::cli_abort(
        "No spreadsheet with that spreadsheet_id found",
        call = rlang::caller_env()
      )
    )
  }
} else if (!is.null(name)) {
  # Handle name
  if (!is.character(name)) {
    cli::cli_abort("{.var name} must be a {.code character}")
  } else {
    r <- googledrive::drive_get({{ name }})
  }
} else if (!is.null(url)) {
  # Handle url
  if (!is.character(url)) {
    cli::cli_abort("{.var url} must be a {.code character}")
  } else {
    # Extract ID from Google Slides URL
    id_pattern <- "https://docs.google.com/spreadsheets/d/([a-zA-Z0-9_-]+)"
    extracted_id <- regmatches(url, regexec(id_pattern, url))[[1]][2]

    if (is.na(extracted_id)) {
      cli::cli_abort(
        "Could not extract spreadsheet ID from URL. Please provide a valid Google Sheets URL."
      )
    }

    r <- tryCatch(
      googledrive::drive_get(id = extracted_id),
      error = \(e)
      cli::cli_abort(
        "No spreadsheet with that URL found",
        call = rlang::caller_env()
      )
    )
  }
}

if (nrow(r) > 1) {
  cli::cli_alert_warning(
    "More than one spreadsheet with that name was found. Please provide a unique name or the spreadsheet_id."
  )
} else {
  if (nrow(r) != 1) {
    cli::cli_abort(
      "No spreadsheet with that name or spreadsheet_id was found. Please provide a valid name or the spreadsheet_id."
    )
  } else {
    id <- r$id
  }
}

# Check if the environment already exists in global when local is FALSE
if (local == FALSE) {
  # Check if the environment already exists and ask to override
  if (exists("google_spreadsheet", envir = .GlobalEnv)) {
    cli::cli_alert_warning(
      "A spreadsheet is already registered in the global environment."
    )

     # Wrap this around logic on only sucsessful finds (see notes above)
    confirmation <- readline("Do you want to override it? (y/n): ")
    if (tolower(confirmation) != "y") {
      cli::cli_alert_info("Registration canceled.")
      return(invisible())
    }
  }

  create_spreadsheet_env_in_global()

  google_spreadsheet$spreadsheet_id <- id
  google_spreadsheet$sheet_ids <- NA
  google_spreadsheet$version <- list(time = Sys.time(), revision_id = NA)

  return(invisible())
} else {
  rtn_env <- new.env()

  rtn_env$spreadsheet_id <- id
  rtn_env$sheet_ids <- NA
  rtn_env$version <- list(time = Sys.time(), revision_id = NA)
  return(rtn_env)
}
}

copy_spreadsheet <- function(spreadsheet) {
  # Copies spreadsheet, displays a message if there are any linked charts and the parent call was NOT copy_linked_presentation
  # Displays a message about overriding the old spreadsheet with the new one in R.
}