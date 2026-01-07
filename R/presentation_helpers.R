#' Validate presentation environment
#'
#' @param presentation A single string naming a presentation environment.
#'
#' @returns
#' Nothing. The function will error if the specified environment doesn't
#' exist in `.GlobalEnv` or if it's missing any of the required objects
#' (`presentation_id`, `slide_ids`, or `current_slide_id`).
#'
#' @keywords internal
validatePresentation <- function(presentation) {
  presentation <- rlang::sym(presentation)

  if (!exists(presentation, envir = .GlobalEnv)) {
    cli::cli_abort(
      c(
        "Invalid or missing presentation.",
        i = "Provide a valid environment.",
        i = "You can create a new presentation or register a presentation to fix this problem."
      ),
      call = rlang::caller_env()
    )
  }

  # Check for required objects
  required_objects <- c("presentation_id", "slide_ids", "current_slide_id")
  missing_objects <- setdiff(required_objects, ls(get(presentation)))

  if (length(missing_objects) > 0) {
    cli::cli_abort(
      c(
        "Your presentation is corrupted or missing.",
        x = "The environment {.var {presentation}} is missing required objects:",
        rlang::set_names(missing_objects, rep("i", length(missing_objects)))
      ),
      call = rlang::caller_env()
    )
  }

  # Future presentation validation checks go here
}


#' Create presentation environment
#'
#' @returns
#' Creates a `google_presentation` environment in the global environment if one does
#' not already exist.
#'
#' @export
create_presentation_env_in_global <- function() {
  if (!exists("google_presentation", envir = .GlobalEnv)) {
    eval(
      quote(.GlobalEnv$google_presentation <- new.env(parent = emptyenv())),
      envir = .GlobalEnv
    )
  }
}

#' @param n A numeric slide ID
#' @rdname on_slide_id
#' @export
on_slide_number <- function(n) {
  cli::cli_abort(c(x = "This function is not yet implemented", i = "Please use {.code on_slide_id()}"))
}

#' @rdname on_slide_id
#' @export
on_current_slide <- function() {
  cli::cli_abort(c(
    x = "This function is not yet implemented",
    i = "Please use {.code on_slide_id()}"
  ))
}

#' @rdname on_slide_id
#' @export
on_newest_slide <- function() {
  cli::cli_abort(c(
    x = "This function is not yet implemented",
    i = "Please use {.code on_slide_id()}"
  ))
}



#' Specify a slide by ID or URL
#'
#' Creates a slide reference object that can be used to target a specific slide
#' in a Google Slides presentation. Accepts either a slide ID (numeric) or a
#' full Google Slides URL.
#'
#' @param id A slide identifier. Can be either:
#'   - A numeric slide ID
#'   - A string containing a numeric slide ID
#'   - A full Google Slides URL
#' 
#' @returns A list containing:
#'   - `presentation_id`: The Google Slides presentation ID
#'   - `slide_id`: The specific slide ID within that presentation
#'
#' @details
#' This function requires a presentation context to be available, either in the
#' calling environment or in the global environment as `google_presentation`.
#' Use [create_presentation_env_in_global()] or register a presentation first.
#'
#' When a URL is provided, the function validates that the presentation ID
#' extracted from the URL matches the currently registered presentation.
#'
#' @examples
#' \dontrun{
#' # Using a numeric slide ID
#' slide_ref <- on_slide_id(6)
#'}
#' 
#' @rdname on_slide_id
#' @export
on_slide_id <- function(id) {

  # If numeric or looks like a numeric string, treat as slide_id
  if (is.numeric(id) || (!grepl("^https?://", id) && !grepl("/", id))) {
   
    if (
      !exists("google_presentation", envir = parent.frame()) &&
        !exists("google_presentation", envir = .GlobalEnv)
    ) {
      cli::cli_abort(
        "No presentation context found. Please register a presentation first."
      )
    }

    slide_obj <- list(
      presentation_id = google_presentation$presentation_id,
      slide_id = id
    )

    return(slide_obj)
  }

  # Parse URL to extract presentation_id and slide_id

  if (!grepl("docs\\.google\\.com/presentation", id)) {
    cli::cli_abort("Invalid Google Slides URL format.")
  }

  # Extract presentation_id
  presentation_match <- regmatches(id, regexec("/presentation/d/([^/]+)", id))
  if (length(presentation_match[[1]]) < 2) {
    cli::cli_abort("Could not extract presentation ID from URL.")
  }
  url_presentation_id <- presentation_match[[1]][2]

  # Extract slide_id from fragment (after #slide=id.)
  slide_match <- regmatches(id, regexec("#slide=id\\.([^&/#]+)", id))
  if (length(slide_match[[1]]) < 2) {
    cli::cli_abort(
      "Could not extract slide ID from URL. Make sure the URL includes '#slide=id.SLIDE_ID'"
    )
  }
  url_slide_id <- slide_match[[1]][2]

  # Get cached presentation_id and validate
  if (
    !exists("google_presentation", envir = parent.frame()) &&
      !exists("google_presentation", envir = .GlobalEnv)
  ) {
    cli::cli_abort(
      "No presentation context found. Please register a presentation first."
    )
  }

  env <- if (exists("google_presentation", envir = parent.frame())) {
    parent.frame()
  } else {
    .GlobalEnv
  }

  cached_presentation_id <- get("google_presentation", envir = env)$presentation_id

  # Validate presentation_id matches
  if (url_presentation_id != cached_presentation_id) {
    cli::cli_abort(c(
      "Presentation ID mismatch.",
      "x" = "This slide belongs to a different presentation than the one you registered in R.",
      "x" = "URL presentation ID: {url_presentation_id}",
      "x" = "Cached presentation ID: {cached_presentation_id}",
      "i" = "Register the new presentation in R with {.code register_presentation()}."
    ))
  }

  return(list(
    presentation_id = url_presentation_id,
    slide_id = url_slide_id
  ))
}

#' @rdname on_slide_id
#' @export
on_slide_url <- on_slide_id