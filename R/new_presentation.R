#' @import R6
#' @import tibble
#' @import png
NULL


#' Create a new Google Slides presentation
#'
#' @param id Optional. A single string. The Google Slides presentation ID.
#' @param title Optional. A single string. The title of the presentation.
#' @param set_active Optional. A logical value indicating whether to set the presentation as the active presentation.
#'
#' @returns A Presentation object
#'
#' @export
new_presentation <- function(
  id = NULL,
  title = 'Untitled Presentation',
  set_active = TRUE
) {
  presentation$new(id = id, title = title, set_active = TRUE)
}


#' Register a Google Slides presentation
#'
#' @description
#' Registers an already created google slides presentation so you can modify it in R.
#' The user supplies either a name, url, or presentation_id. Supplying multiple will cause
#' the function to error. If multiple presentation match a given identifier (only possible for name)
#' the function presents you with a dribble (see the `googledrive` package) and ask which one you want.
#'
#' @param id Optional. A single string. The Google Slides presentation ID.
#' @param title Optional. A single string. The title of the presentation.
#' @param set_active Optional. A logical value indicating whether to set the presentation as the active presentation.
#'
#' @returns A Presentation object
#' @export
register_presentation <- function(
  id = NULL,
  title = 'Untitled Presentation',
  set_active = TRUE
) {
  presentation$new(id = id, title = title, set_active = TRUE)
}

#' R6 Class for Google Slides Presentations
#'
#' @description
#' An R6 class to represent and manipulate Google Slides presentations.
#'
#' @export
presentation <- R6::R6Class(
  "presentation",

  cloneable = FALSE,

  # Public fields and methods
  public = list(
    #' @field title The title of the presentation
    title = NULL,

    #' @field presentation_id The Google Slides presentation ID
    presentation_id = NULL,

    #' @field page_size Dimensions of slides (height and width in PTs)
    page_size = NULL,

    #' @field revision_id The current revision ID from Google Drive
    revision_id = NULL,

    #' @field locale The presentation locale (e.g., "en")
    locale = NULL,

    #' @field masters List of master slide metadata
    masters = NULL,

    #' @field layouts List of layout slide metadata
    layouts = NULL,

    #' @field last_refreshed Timestamp of last refresh from API
    last_refreshed = NULL,

    #' @description
    #' Create or open a presentation
    #'
    #' @param id Presentation ID, URL, name, or NULL to create new
    #' @param title Title for new presentation (only used if id is NULL)
    #' @param set_active Whether to make this the active presentation
    #'
    #' @return A new `presentation` object
    initialize = function(
      id = NULL,
      title = "Untitled Presentation",
      set_active = TRUE
    ) {
      if (is.null(id)) {
        # Create new presentation
        private$create_new(title)
      } else {
        # Open existing presentation
        private$open_existing(id)
      }

      # Set as active presentation if requested
      if (set_active) {
        if (active_presentation_exists()) {
          # Ask for confirmation
          confirmation <- readline(
            "An active presentation is already registered. Do you want to override it? (y/n): "
          )
          if (tolower(confirmation) != "y") {
            cli::cli_alert_info(
              "Registration canceled, returning presentation without setting as active."
            )
            return(invisible(self))
          }
        } else {
          self$set_active()
        }
      }

      invisible(self)
    },

    #' @description
    #' Refresh presentation data from Google Slides API
    #'
    #' @return Self, invisibly (for method chaining)
    refresh = function() {
      if (is.null(self$presentation_id)) {
        cli::cli_abort("Cannot refresh: presentation_id is not set")
      }

      # Fetch current state from API
      rsp <- query(
        endpoint = "slides.presentations.get",
        params = list(presentationId = self$presentation_id),
        base = "slides"
      )

      # Update all fields
      private$populate_from_response(rsp)

      cli::cli_alert_success("Refreshed presentation {.val {self$title}}")

      invisible(self)
    },

    #' @description
    #' Delete the presentation from Google Drive
    #'
    #' @param permanent Whether to permanently delete (TRUE) or move to trash (FALSE)
    #' @return NULL, invisibly
    delete = function() {
      if (is.null(self$presentation_id)) {
        cli::cli_abort("Cannot delete: presentation_id is not set")
      }

      if (exists(".auth", envir = .GlobalEnv)) {
        if (inherits(.auth, "AuthState")) {
          if (is.null(.auth$credentials)) {
            r2slides_auth()
          }
        }
      }

      googledrive::drive_trash(googledrive::as_id(self$presentation_id))
      cli::cli_alert_success(
        "Moved presentation {.val {self$title}} to trash"
      )

      # Deactivate if this was active
      if (private$is_active_flag) {
        private$is_active_flag <- FALSE
      }

      invisible(NULL)
    },

    #' @description
    #' Create a copy of the presentation
    #'
    #' @param name Name for the copy (defaults to "Copy of original_name")
    #' @return A new presentation object for the copy
    copy = function(name = NULL) {
      if (is.null(self$presentation_id)) {
        cli::cli_abort("Cannot copy: presentation_id is not set")
      }

      if (exists(".auth", envir = .GlobalEnv)) {
        if (inherits(.auth, "AuthState")) {
          if (is.null(.auth$credentials)) {
            r2slides_auth()
          }
        }
      }

      # Determine copy name
      copy_name <- name %||% paste("Copy of", self$title)

      # Create copy via Drive API
      copied_file <- googledrive::drive_cp(
        file = googledrive::as_id(self$presentation_id),
        name = copy_name
      )

      cli::cli_alert_success("Created copy: {.val {copy_name}}")

      # Return new presentation object for the copy
      presentation$new(id = copied_file$id, set_active = FALSE)
    },

    #' @description
    #' Get slide object from the presentation
    #'
    #' @param index Index of the slide to return (1-based)
    #' @return A slide object
    get_slide_by_index = function(index) {
      if (is.null(private$slide_ids) || length(private$slide_ids) == 0) {
        cli::cli_alert_warning(
          "No slides available. Try calling $refresh() first."
        )
        return(NULL)
      }

      # Require index
      if (is.null(index)) {
        cli::cli_abort("{.arg index} is required")
      }

      # Validate index is a single integer
      if (length(index) != 1) {
        cli::cli_abort("{.arg index} must be a single integer")
      }

      index <- suppressWarnings(as.integer(index))

      if (is.na(index)) {
        cli::cli_abort("{.arg index} must be a single integer")
      }

      # Validate index bounds
      if (index < 1 || index > length(private$slide_ids)) {
        cli::cli_abort(
          "Index out of bounds. Presentation has {length(private$slide_ids)} slide{?s}."
        )
      }

      # Return slide object constructed with presentation reference and slide_id
      slide(presentation = self, slide_id = private$slide_ids[[index]])
    },

    #' @description
    #' Get slide object from the presentation
    #'
    #' @param slide_id ID of the slide to return
    #' @return A slide object
    get_slide_by_id = function(slide_id) {
       if (is.null(private$slide_ids) || length(private$slide_ids) == 0) {
        cli::cli_alert_warning(
          "No slides available. Try calling $refresh() first."
        )
        return(NULL)
      }

      # Require slide_id
      if (is.null(slide_id)) {
        cli::cli_abort("{.arg slide_id} is required")
      }

      # Validate slide_id is a single string
      if (length(slide_id) != 1) {
        cli::cli_abort("{.arg slide_id} must be a single string")
      }

      # validate character
      if (!is.character(slide_id)) {
        cli::cli_abort("{.arg slide_id} must be a single string")
      }

      # Check if slide_id exists
      if (!slide_id %in% private$slide_ids) {
        cli::cli_abort("Slide ID not found in this presentation")
      }

      # Return slide object constructed with presentation reference and slide_id
      slide(presentation = self, slide_id = slide_id)
    },
      
      
    #' @description
    #' Get the position of a slide in the presentation
    #'
    #' @param slide A slide object
    #' @return Index of the slide
    get_slide_index = function(slide) {
      if (!is.slide(slide)) {
        cli::cli_abort("{.arg slide} must be a slide object")
      }

      # Find the position of this slide's ID in the vector
      index <- match(slide@slide_id, private$slide_ids)

      if (is.na(index)) {
        cli::cli_abort("Slide not found in this presentation")
      }

      index
    },
    #' @description
    #' Get slide IDs from the presentation
    #'
    #' @return Character vector of slide object IDs
    get_slide_ids = function() {
      private$slide_ids %||% character(0)
    },

    #' @description
    #' Set this presentation as the active one
    #'
    #' @return Self, invisibly (for method chaining)
    set_active = function() {
      # Deactivate any currently active presentation
      if (active_presentation_exists()) {
        if (!identical(get_active_presentation(), self)) {
          active$set_not_active()
        }
      }

      # Set this as active
      private$is_active_flag <- TRUE

      .r2slides_objects$active_presentation <- self

      invisible(self)
    },

    #' @description
    #' Set this presentation to not active
    #'
    #' @return Self, invisibly (for method chaining)
    set_not_active = function() {
      private$is_active_flag <- FALSE

      if (active_presentation_exists()) {
        assign("active_presentation", NULL, envir = .r2slides_objects)
      }

      invisible(self)
    },

    #' @description
    #' Check if this is the active presentation
    #'
    #' @return Logical
    is_active = function() {
      private$is_active_flag
    },

    #' @description
    #' Get the Google Slides URL for this presentation
    #'
    #' @return Character URL or NULL
    get_url = function() {
      if (is.null(self$presentation_id)) {
        return(NULL)
      }
      paste0("https://docs.google.com/presentation/d/", self$presentation_id)
    },

    #' @description
    #' Open the presentation in a browser
    #'
    #' @return Self, invisibly (for method chaining)
    browse = function() {
      url <- self$get_url()
      if (is.null(url)) {
        cli::cli_abort("Cannot browse: presentation_id is not set")
      }

      utils::browseURL(url)
      invisible(self)
    },

    #' Get elements from the presentation
    #'
    #' @description
    #' Filter the elements you ahve constructed to return a list of elements
    #'
    #' @param modified_since Optional. Only return elements modified since this time
    #' @param modified_end Optional. Only return elements modified before this time
    #' @param created_since Optional. Only return elements created since this time
    #' @param created_end Optional. Only return elements created before this time
    #' @param element_type Optional. Only return elements of this type
    #' @param element_text Optional. Only return elements with this text
    #' @param show_deleted Optional. Show deleted elements
    #'
    #' @return List of elements
    get_elements = function(
      modified_since = NULL,
      modified_end = NULL,
      created_since = NULL,
      created_end = NULL,
      element_type = NULL,
      element_text = NULL,
      show_deleted = FALSE
    ) {
      modify_since <- modified_since %|% "1970-01-01T00:00:00.000Z"
      modify_end <- modified_end %|% Sys.time()
      create_since <- created_since %|% "1970-01-01T00:00:00.000Z"
      create_end <- created_end %|% Sys.time()

      ldgr <- private$ledger |>
        dplyr::filter(
          time_updated > modified_since,
          time_updated < modified_end,
          time_created > created_since,
          time_created < created_end
        )

      if (!null(element_type)) {
        ldgr <- ldgr |>
          dplyr::filter(element_type == element_type)
      }

      if (!null(element_text)) {
        ldgr <- ldgr |>
          dplyr::filter(element_text == element_text)
      }

      if (!show_deleted) {
        ldgr <- ldgr |>
          dplyr::filter_out(is_deleted)
      }

      if (nrow(ldgr) == 0) {
        return(NULL)
      }

      element_ids <- ldgr$element_id
      slide_ids <- ldgr$slide_id

      elements_rtn <- list(
        element_id = element_ids,
        slide_id = slide_ids
      ) |>
        transpose()

      return(elements_rtn)
    },

    #' @description
    #' Add an element to the ledger
    #'
    #' @param element_id Element ID
    #' @param slide_id Slide ID
    #' @param element_type Element type
    #' @param element_text Element text
    #'
    #' @return Self
    add_to_ledger = function(element_id, slide_id, element_type, element_text) {
      private$ledger <- dplyr::bind_rows(
        private$ledger,
        tibble::tibble_row(
          element_id = element_id,
          slide_id = slide_id,
          element_type = element_type,
          element_text = element_text,
          time_created = Sys.time(),
          time_updated = Sys.time(),
          time_deleted = NA,
          is_deleted = FALSE,
          time_known_deletion = NA
        )
      )
    },

    #' @description
    #' Print method for presentation objects
    #'
    #' @param ... Additional arguments (unused)
    print = function(...) {
      cli::cli_h2("Google Slides Presentation")
      cli::cli_dl(c(
        "Title" = self$title %||% "{.emph Not set}",
        "ID" = self$presentation_id %||% "{.emph Not set}",
        "Slides" = as.character(length(self$slides) %||% 0),
        "Active" = if (private$is_active_flag) "{.strong Yes}" else "No",
        "Last refreshed" = if (!is.null(self$last_refreshed)) {
          format(self$last_refreshed, "%Y-%m-%d %H:%M:%S")
        } else {
          "{.emph Never}"
        }
      ))

      if (!is.null(self$presentation_id)) {
        cli::cli_text("{.url {self$get_url()}}")
      }

      invisible(self)
    }
  ),

  # Private fields and methods
  private = list(
    # Whether this presentation is currently active
    is_active_flag = FALSE,

    # Raw data for the slides
    slides = NULL,

    # Slide ID's
    slide_ids = NULL,

    # Create a new presentation via API
    create_new = function(title) {
      # Validate inputs
      if (!is.character(title) || length(title) != 1) {
        cli::cli_abort("{.arg title} must be a single string")
      }

      # Create the presentation
      create_body <- list(title = title)

      # Get rid of the FALSE when we have test_rsp
      rsp <- if (is_testing()) {
        test_rsp
      } else {
        query(
          endpoint = "slides.presentations.create",
          body = create_body,
          base = "slides"
        )
      }

      # Populate fields from response
      private$populate_from_response(rsp)

      cli::cli_alert_success("Created presentation {.val {title}}")

      invisible(self)
    },

    # Open an existing presentation
    open_existing = function(id) {
      # Determine what type of identifier we have
      presentation_id <- resolve_presentation_id(id)

      # Fetch presentation data
      rsp <- query(
        endpoint = "slides.presentations.get",
        params = list(presentationId = presentation_id),
        base = "slides"
      )

      # Populate fields from response
      private$populate_from_response(rsp)

      cli::cli_alert_success("Opened presentation {.val {self$title}}")

      invisible(self)
    },

    ledger = tibble::tibble(
      element_id = character(),
      slide_id = character(),
      element_type = character(),
      element_text = character(),
      time_created = NULL,
      time_updated = NULL,
      time_deleted = NULL,
      is_deleted = logical(),
      time_known_deletion = NULL
    ),

    refresh_ledger = function() {
      # Check that the elements exist. If not, add is_deleted and time_known_deletion
      NULL
    },

    # Populate object fields from API response
    populate_from_response = function(rsp) {
      self$title <- rsp$title
      self$presentation_id <- rsp$presentationId
      self$revision_id <- rsp$revisionId
      self$locale <- rsp$locale
      self$page_size <- rsp$pageSize

      self$masters <- rsp$masters
      self$layouts <- rsp$layouts
      self$last_refreshed <- Sys.time()

      private$slides <- rsp$slides
      private$slide_ids <- purrr::map(private$slides, ~ .x$objectId)
    },

    # Set finalizer
    finalize = function() {
      if (private$is_active_flag) {
        self$set_not_active()
      }
    }
  )
)


#' is.presentation
#'
#' @description
#' Check if an object is a presentation
#'
#' @param x An object to check
#'
#' @return `TRUE` if `x` is a `presentation`, `FALSE` otherwise
#'
#' @export
is.presentation <- function(x) {
  inherits(x, "presentation")
}

#' Get the currently active presentation
#'
#' @return A `presentation` object, or errors if none active
#' @export
get_active_presentation <- function() {
  if (active_presentation_exists()) {
    get("active_presentation", envir = .r2slides_objects)
  } else {
    cli::cli_abort("No active presentation")
  }
}

active_presentation_exists <- function() {
  if (exists("active_presentation", envir = .r2slides_objects)) {
    is.presentation(get("active_presentation", envir = .r2slides_objects))
  } else {
    FALSE
  }
}
