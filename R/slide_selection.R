#' Slide Object
#'
#' An object representing a Google Slides slide/presentation combo
#' @param presentation A presentation object
#' @param slide_id A slide ID
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
          recursivly_replace('speakerNotesObjectId', '') |>
          purrr::modify_tree(
            leaf = function(leaf) {
              if (is.numeric(leaf)) {
                round(leaf, digits = 3)
              } else {
                leaf
              }
            }
          ) |>
          rlang::hash()
      }
    ),

    elements_raw = S7::new_property(
      S7::class_character,
      getter = function(self) {
        query(
          endpoint = "slides.presentations.pages.get",
          params = list(
            presentationId = self@presentation$presentation_id,
            pageObjectId = self@slide_id
          ),
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

#' Create a slide object
#'
#' Creates a slide reference object that can be used to target a specific slide
#' in a Google Slides presentation. Accepts either a slide ID or a
#' full Google Slides URL.
#'
#' @param id A slide identifier. Can be either:
#'   - A numeric slide ID
#'   - A string containing slide ID
#'   - A full Google Slides URL
#'
#' @returns A slide object
#'
#' @examples
#' \dontrun{
#' # Using slide ID
#' slide_ref <- on_slide_id('f82nannfsl_0')
#'}
#'
#' @rdname on_slide_id
#' @export
on_slide_id <- function(id, ps) {
  if (rlang::is_missing(ps)) {
    ps <- get_active_presentation()
  }

  if (length(id) > 1 || is.null(id) || is.na(id)) {
    cli::cli_abort("{.arg id} must be a single value")
  }

  ps$get_slide_by_id(slide_id = id)
}

#' @param url A URL pointing to a Google Slides slide
#' @rdname on_slide_id
#' @export
on_slide_url <- function(url, ps) {
  if (rlang::is_missing(ps)) {
    pres_id <- resolve_presentation_id(url)
  } else {
    pres_id <- ps$presentation_id
  }
  if (active_presentation_exists()) {
    ps <- get_active_presentation()
    if (ps$presentation_id != pres_id) {
      cli::cli_warn(
        c("x" = "Active presentation ({.val {ps$presentation_id}}) is not the same presentation as ({.val {url}})",
        "i" = "Get the active presentation with {.code get_active_presentation()}")
      )
      flush.console()
      confirmation <- readline(
        "Would you like to override the active presentation with the presentation that matches this presentation? (y/n): "
      )
      if (tolower(confirmation) != "y") {
        cli::cli_alert_info(
          "Did not override the active presentation."
        )
      } else {
        register_presentation(pres_id)
        ps <- get_active_presentation()
      }
    }
  } else {
    register_presentation(pres_id)
    ps <- get_active_presentation()
  }
  ps$get_slide_by_id(slide_id = resolve_slide_id(url, pres_id))
}


#' @param n A numeric slide ID
#' @param ps A presentation object
#' @rdname on_slide_id
#' @export
on_slide_number <- function(n, ps) {
  if (rlang::is_missing(ps)) {
    ps <- get_active_presentation()
  }
  if (length(n) > 1 || is.null(n) || is.na(n) || !is.numeric(n)) {
    cli::cli_abort("{.arg n} must be a single numeric value")
  }
  ps$get_slide_by_index(index = n)
}

#' @param slide A slide object
#' @param ps A presentation object
#' @param offset A position integer - the number of slides after the reference slide. Can be negative to return slides before the reference slide
#' @rdname on_slide_id
#' @export
on_slide_after <- function(slide, offset = 1, ps) {
  if (rlang::is_missing(ps)) {
    ps <- get_active_presentation()
  }

  idx <- ps$get_slide_index(slide) + offset
  ps$get_slide_by_index(idx)
}


resolve_presentation_id <- function(id) {
  if (!is.character(id) || length(id) != 1) {
    cli::cli_abort("{.arg id} must be a single string")
  }

  # Check if it's a URL
  if (grepl("^https://docs\\.google\\.com/presentation", id)) {
    id_pattern <- "https://docs\\.google\\.com/presentation/d/([a-zA-Z0-9_-]+)"
    matches <- regmatches(id, regexec(id_pattern, id))[[1]]

    if (length(matches) < 2 || is.na(matches[2])) {
      cli::cli_abort("Could not extract presentation ID from URL")
    }

    id <- matches[2]
  }

  # Check if it looks like a presentation ID (alphanumeric with hyphens/underscores)
  if (grepl("^[a-zA-Z0-9_-]+$", id)) {
    # Try to fetch it directly - if it works, it's an ID

    test_fetch <- tryCatch(
      {
        query(
          endpoint = "slides.presentations.get",
          params = list(presentationId = id),
          base = "slides"
        )
        TRUE
      },
      error = function(e) {
        FALSE
      }
    )
    if (test_fetch) {
      return(id)
    }
  }

  if (exists(".auth", envir = .GlobalEnv)) {
    if (inherits(.auth, "AuthState")) {
      if (is.null(.auth$credentials)) {
        r2slides_auth()
      }
    }
  }

  # Otherwise, treat it as a name and search Drive
  drive_file <- googledrive::drive_find(
    pattern = paste0(
      "^",
      gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", id),
      "$"
    ),
    type = "presentation",
    n_max = 50
  )

  if (nrow(drive_file) == 0) {
    cli::cli_abort("No presentation found with name {.val {id}}")
  }

  if (nrow(drive_file) > 1) {
    cli::cli_abort(c(
      "Multiple presentations found with name {.val {id}}",
      "i" = "Use the presentation ID or URL for a unique identifier",
      "i" = "Found {nrow(drive_file)} presentation{?s}"
    ))
  }

  drive_file$id[1]
}

resolve_slide_id <- function(id, presentation_id) {
  if (!is.character(id) || length(id) != 1) {
    cli::cli_abort("{.arg id} must be a single string")
  }

  if (!is.character(presentation_id) || length(presentation_id) != 1) {
    cli::cli_abort("{.arg presentation_id} must be a single string")
  }

  # Check if it's a URL
  if (grepl("^https://docs\\.google\\.com/presentation", id)) {
    # Pattern for slide URLs: .../d/{presentationId}/edit#slide=id.{slideId}
    slide_pattern <- "#slide=id\\.([a-zA-Z0-9_-]+)"
    matches <- regmatches(id, regexec(slide_pattern, id))[[1]]

    if (length(matches) < 2 || is.na(matches[2])) {
      cli::cli_abort("Could not extract slide ID from URL")
    }

    # Verify the presentation ID matches
    # if (matches[2] != presentation_id) {
    #   cli::cli_abort("Slide URL belongs to a different presentation")
    # }

    id <- matches[2]
  }

  # Check if it looks like a slide ID (alphanumeric with hyphens/underscores)
  if (grepl("^[a-zA-Z0-9_-]+$", id)) {
    # Verify slide exists in the presentation
    rsp <- tryCatch(
      {
        query(
          endpoint = "slides.presentations.pages.get",
          params = list(
            presentationId = presentation_id,
            pageObjectId = id
          ),
          base = "slides"
        )
      },
      error = function(e) {
        cli::cli_abort("Slide {.val {id}} not found in presentation")
      }
    )

    return(id)
  }

  cli::cli_abort("Invalid slide ID format: {.val {id}}")
}


#' Check if an object is a slide
#'
#' @param x An object to check
#'
#' @return TRUE if the object is a slide, FALSE otherwise
#' @export
is.slide <- function(x) {
  inherits(x, "r2slides::slide")
}
