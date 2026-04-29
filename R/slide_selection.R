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
    presentation = S7::new_property(setter = NULL, validator = function(value) {
      if (!is.presentation(value)) {
        "presentation must be of class `presentation`"
      }
    }),

    slide_id = S7::new_property(
      class_character,
      setter = NULL,
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
          recursively_replace('objectId', '') |>
          recursively_replace('speakerNotesObjectId', '') |>
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

  validator = function(self) {
    if (!self@slide_id %in% unlist(self@presentation$get_slide_ids_cache())) {
      glue::glue(
        "Slide '{self@slide_id}' does not exist in presentation '{self@presentation$title}'."
      )
    }
  },

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

    if (active_presentation_exists()) {
      ps <- get_active_presentation()
      if (ps$presentation_id != pres_id) {
        cli::cli_warn(c(
          "x" = "Active presentation ({.val {ps$presentation_id}}) does not match the URL ({.val {pres_id}})",
          "i" = "Get the active presentation with {.code get_active_presentation()}"
        ))
        utils::flush.console()
        confirmation <- readline(
          "Would you like to override the active presentation with the presentation that matches this URL? (y/n): "
        )
        if (tolower(confirmation) == "y") {
          register_presentation(pres_id)
          ps <- get_active_presentation()
        }
      }
    } else {
      register_presentation(pres_id)
      ps <- get_active_presentation()
    }
  } else {
    if (active_presentation_exists()) {
      active <- get_active_presentation()
      if (active$presentation_id != ps$presentation_id) {
        cli::cli_warn(c(
          "x" = "The active presentation ({.val {active$presentation_id}}) is not the presentation passed to {.fn on_slide_url}",
          "i" = "Set the correct active presentation with {.code register_presentation()}"
        ))
      }
    }
  }
  ps$get_slide_by_id(slide_id = resolve_slide_id(url, ps$presentation_id))
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


#' @param text A string to search for in slide speaker notes
#' @param match How to match \code{text} against notes: \code{"exact"} requires
#'   an identical string; \code{"regex"} treats \code{text} as a Perl-compatible
#'   regular expression
#' @param on_multiple What to do when multiple slides match: \code{"error"}
#'   (default) raises an error listing the matching slide numbers; \code{"return"}
#'   returns all matches as a named list of slide objects (names are slide IDs)
#' @rdname on_slide_id
#' @export
on_slide_with_notes <- function(
  text,
  match = c("exact", "regex"),
  on_multiple = c("error", "return"),
  ps
) {
  if (rlang::is_missing(ps)) {
    ps <- get_active_presentation()
  }

  match       <- rlang::arg_match(match)
  on_multiple <- rlang::arg_match(on_multiple)

  if (!is.character(text) || length(text) != 1 || is.na(text)) {
    cli::cli_abort("{.arg text} must be a single non-NA string")
  }


  slide_ids <- ps$get_slide_ids_cache()

  if (length(slide_ids) == 0) {
    cli::cli_abort("Presentation has no slides")
  }

  notes <- purrr::map_chr(slide_ids, ~ ps$get_slide_notes_text(.x))

  matched <- if (match == "exact") {
    notes == text
  } else {
    grepl(text, notes, perl = TRUE)
  }

  n_matched <- sum(matched)

  if (n_matched == 0) {
    cli::cli_abort(
      "No slides found whose notes {if (match == 'exact') 'equal' else 'match'} {.val {text}}"
    )
  }

  matched_ids     <- slide_ids[matched]
  matched_indices <- which(matched)

  # Construct slide objects directly to avoid a refresh() per matched slide
  matched_slides <- purrr::map(matched_ids, ~ slide(presentation = ps, slide_id = .x))

  if (on_multiple == "error") {
    if (n_matched > 1) {
      cli::cli_abort(c(
        "{n_matched} slides found whose notes {if (match == 'exact') 'equal' else 'match'} {.val {text}}",
        "i" = "Matched slide numbers: {matched_indices}",
        "i" = "Use {.code on_multiple = 'return'} to retrieve all matches"
      ))
    }
    return(matched_slides[[1]])
  }

  # on_multiple == "return"
  rlang::set_names(matched_slides, matched_ids)
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
