#' Create a new Google Slides presentation
#'
#' @param name A single string specifying the title of the presentation.
#' @param location Optional. A folder location for where to create the presentation. Defaults to your My Drive Home.
#' @param verbose Optional. A logical indicating whether to print status updates. (DEV)
#' @param overwrite Optional. A logical indicating whether to overwrite existing presentations. Defaults to FALSE.
#' @param ... Additional arguments reserved for future expansion
#'
#' @returns
#' Creates a new Google Slides presentation and modifies the global environment.
#' Returns invisibly.
#'
#' @export
new_presentation <- function(
    name,
    location = NULL,
    verbose = TRUE,
    overwrite = FALSE,
    ...
) {
  #Checks if a presentation already has that name, and ask for confirmation if that is the case
  #Creates a new Slides Presentation

  #Validate inputs
  #Validate token

  #Implement Default location

  #For testing purposes, set overwrite to be TRUE
  overwrite <- TRUE

  if (overwrite == FALSE) {
    #Get any presentations with that name
    #Ask for clarification if any are found
  } else {
    slides_api_url <- "https://slides.googleapis.com/v1/presentations"

    access_token <- token$credentials$access_token

    create_presentation <- list(
      title = {{ name }}
    )

    rsp <- query(
      url = slides_api_url,
      access_token = access_token,
      body = create_presentation
    )

    #Print status update
    if (verbose == TRUE) {
      print(rsp)
    }

    presentation_info <- resp_body_json(rsp)

    create_presentation_env_in_global()

    google_presentation$presentation_id <- presentation_info$presentationId
    google_presentation$slide_ids <- NA
    google_presentation$current_slide_id <- NA
  }
  #return invisible?
}



#' Register a Google Slides presentation
#'
#' @description
#' Registers an already created google slides presentation so you can modify it in R.
#' The user supplies either a name, url, or presentation_id. Supplying multiple will cause
#' the function to error. If multiple presentation match a given identifier (only possible for name)
#' the function presents you with a dribble (see the `googledrive` package) and ask which one you want.
#'
#' @param name Optional. A single string. The name of the presentation in Google Drive.
#' @param url Optional. A single string. The URL of the Google Slides presentation.
#' @param presentation_id Optional. A single string. The Google Slides presentation ID.
#' @param local Optional. A logical value indicating whether to register the presentation in a new environment.
#' @param ... Currently unused; must be empty.
#'
#' @returns
#' If `local = TRUE`, returns a new environment containing presentation details.
#' If `local = FALSE`, modifies the global environment and returns invisibly.
#' Will error if more than one identifier is provided or if no presentation is found.
#'
#' @export
register_presentation <- function(
    name = NULL,
    url = NULL,
    presentation_id = NULL,
    local = FALSE,
    ...
) {
  #Registers a presentation
  #Name or presentation_id of a slides presentation in your google drive
  #Registers that presentation in R
  #Can register the presentation to the global env or a new env (to have the ability to edit multiple presentations at the same time)

  # Check if more than one identifier is provided
  identifiers_provided <- sum(
    !is.null(name),
    !is.null(url),
    !is.null(presentation_id)
  )
  if (identifiers_provided > 1) {
    cli::cli_abort(
      "Please provide only one of {.var name}, {.var url}, or {.var presentation_id}"
    )
  }

  # Handle presentation_id
  if (!is.null(presentation_id)) {
    if (!is.character(presentation_id)) {
      cli::cli_abort("{.var presentation_id} must be a {.code character}")
    } else {
      r <- tryCatch(
        drive_get(id = presentation_id),
        error = \(e)
        cli::cli_abort(
          "No presentation with that presentation_id found",
          call = caller_env()
        )
      )
    }
  } else if (!is.null(name)) {
    # Handle name
    if (!is.character(name)) {
      cli::cli_abort("{.var name} must be a {.code character}")
    } else {
      r <- drive_get({{ name }})
    }
  } else if (!is.null(url)) {
    # Handle url
    if (!is.character(url)) {
      cli::cli_abort("{.var url} must be a {.code character}")
    } else {
      # Extract ID from Google Slides URL
      id_pattern <- "https://docs.google.com/presentation/d/([a-zA-Z0-9_-]+)"
      extracted_id <- regmatches(url, regexec(id_pattern, url))[[1]][2]

      if (is.na(extracted_id)) {
        cli::cli_abort(
          "Could not extract presentation ID from URL. Please provide a valid Google Slides URL."
        )
      }

      r <- tryCatch(
        drive_get(id = extracted_id),
        error = \(e)
        cli::cli_abort(
          "No presentation with that URL found",
          call = caller_env()
        )
      )
    }
  } else {
    # No identifier provided
    cli::cli_abort(
      "Please provide one of {.var name}, {.var url}, or {.var presentation_id}"
    )
  }

  if (nrow(r) > 1) {
    cli::cli_alert_warning(
      "More than one presentation with that name was found. Please provide a unique name or the presentation_id."
    )
  } else {
    if (nrow(r) != 1) {
      cli::cli_alert_warning(
        "No presentation with that name or presentation_id was found. Please provide a valid name or the presentation_id."
      )
    } else {
      id <- r$id
    }
  }

  # Check if the environment already exists in global when local is FALSE
  if (local == FALSE) {
    # Check if the environment already exists and ask to override
    if (exists("google_presentation", envir = .GlobalEnv)) {
      cli::cli_alert_warning(
        "A presentation is already registered in the global environment."
      )

      confirmation <- readline("Do you want to override it? (y/n): ")
      if (tolower(confirmation) != "y") {
        cli::cli_alert_info("Registration canceled.")
        return(invisible())
      }
    }

    create_presentation_env_in_global()

    google_presentation$presentation_id <- id
    google_presentation$slide_ids <- NA
    google_presentation$current_slide_id <- NA

    return(invisible())
  } else {
    rtn_env <- new.env()

    rtn_env$presentation_id <- id
    rtn_env$slide_ids <- NA
    rtn_env$current_slide_id <- NA

    return(rtn_env)
  }
}
