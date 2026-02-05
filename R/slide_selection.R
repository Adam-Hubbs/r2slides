#' @param n A numeric slide ID
#' @rdname on_slide_id
#' @export
on_slide_number <- function(n) {
  cli::cli_abort(c(
    x = "This function is not yet implemented",
    i = "Please use {.code on_slide_id()}"
  ))
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


#' Slide Object
#'
#' An object representing a Google Slides slide/presentation combo
#'
#'
#' @export
slide <- S7::new_class(
  "slide",
  properties = list(
    presentation = S7::new_property(validator = function(value) {
      if (!is.presentation(value)) {
        "presentation must be of class `presentation`"
      }
    }),

    slide_id = S7::new_property(
      class_character,
      validator = function(value) {
      if (length(value) != 1) {
        "slide_id must be a single value"
      }
    }
  ),

    # Computed properties
    slide_hash = S7::new_property(
      S7::class_character,
      getter = function(self) {
        self@elements_raw |>
          recursivly_replace('objectId', '') |>
          rlang::hash()
      }
    ),

    elements_raw = S7::new_property(
      S7::class_character,
      getter = function(self) {
              query(
                endpoint = "slides.presentations.pages.get",
                params = list(presentationId = self@presentation$presentation_id,
                              pageObjectId = self@slide_id),
                base = "slides"
              )
                
      }
    )
  ),
  constructor = function(presentation, slide_id) {
    S7::new_object(
      S7::S7_object(),
      presentation = presentation,
      slide_id = slide_id
    )
  }
)

# Check that this works
S7::method(`==`, list(slide, slide)) <- function(e1, e2) {
    return(e1@slide_hash == e2@slide_hash)
}

S7::method(print, slide) <- function(x, ...) {
  cli::cli_h2("Google Slides Slide Object")
  cli::cli_text("Slide ID: {.val {x@slide_id}}")
  cli::cli_text("Presentation ID: {.val {x@presentation$presentation_id}}")
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
#' @returns A slide object
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
    pres <- get_active_presentation()

    slide_obj <- list(
      presentation_id = pres$presentation_id,
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

  cached_presentation_id <- get_active_presentation()$presentation_id

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

