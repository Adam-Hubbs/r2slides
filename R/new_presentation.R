#' Create a new Google Slides presentation
#'
#' @param name A single string specifying the title of the presentation.
#' @param location Optional. A folder location for where to create the presentation. Defaults to your My Drive Home.
#' @param verbose Optional. A logical indicating whether to print status updates. (DEV)
#' @param overwrite Optional. A logical indicating whether to overwrite existing presentations. Defaults to FALSE. (TRUE for testing purposes)
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
  overwrite = FALSE, #Set to TRUE for Testing purposes
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
    create_presentation <- list(
      title = {{ name }}
    )

    rsp <- query(
      endpoint = 'slides.presentations.create',
      body = create_presentation,
      base = 'slides',
      #call = #Leave null so caller_env points to this function as the lowest exported function?
    )

    create_presentation_env_in_global()
    google_presentation$raw <- rsp
    google_presentation$presentation_id <- rsp$presentationId
    google_presentation$slide_ids <- lapply(rsp$slides, function(slide) {
      slide$objectId
    })
    google_presentation$current_slide_id <- google_presentation$slide_ids[[length(
      google_presentation$slide_ids
    )]]
    google_presentation$version <- list(
      time = Sys.time(),
      revision_id = rsp$revisionId
    )
    google_presentation$slide_size <- rsp$pageSize
  }

  # Currently this does not set all of the meta-data needed. In particular it does not get the slide id of the one default slide.
  # Because of this, currently one needs to call new_slide() before any functions that add text or images so that the text/image setting functions have a slide_id to grab.

  invisible()
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
  } else if (identifiers_provided == 0) {
    # No identifier provided
    cli::cli_abort(
      "Please provide one of {.var name}, {.var url}, or {.var presentation_id}"
    )
  }

  # Pass authentication tokens to googledrive and googlesheets4
  pass_client <- r2slides_token()$auth_token$client
  googledrive::drive_auth_configure(client = pass_client)
  googlesheets4::gs4_auth_configure(client = pass_client)

  # Error handling flow when no files found by drive needs updating. Currently drive puts out warnings which get printed then we error later.
  # Need to catch the googledrive warnings, wrap our error around, then print googledrive errors (after turning them into errors).

  # Also need to improve error handling when multiple files have the same name and match the name argument of register_presentation.
  # Currently gives warnings and then the readLines for overriding. Do not override unless we have a match.

  # Handle presentation_id
  if (!is.null(presentation_id)) {
    if (!is.character(presentation_id)) {
      cli::cli_abort("{.var presentation_id} must be a {.code character}")
    } else {
      r <- tryCatch(
        googledrive::drive_get(id = presentation_id),
        error = \(e) {
          cli::cli_abort(
            "No presentation with that presentation_id found",
            call = rlang::caller_env()
          )
        }
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
      id_pattern <- "https://docs.google.com/presentation/d/([a-zA-Z0-9_-]+)"
      extracted_id <- regmatches(url, regexec(id_pattern, url))[[1]][2]

      if (is.na(extracted_id)) {
        cli::cli_abort(
          "Could not extract presentation ID from URL. Please provide a valid Google Slides URL."
        )
      }

      r <- tryCatch(
        googledrive::drive_get(id = extracted_id),
        error = \(e) {
          cli::cli_abort(
            "No presentation with that URL found",
            call = rlang::caller_env()
          )
        }
      )
    }
  }

  if (nrow(r) > 1) {
    cli::cli_alert_warning(
      "More than one presentation with that name was found. Please provide a unique name or the presentation_id."
    )
  } else {
    if (nrow(r) != 1) {
      cli::cli_abort(
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

      # Wrap this around logic on only sucsessful finds (see notes above)
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
    google_presentation$version <- list(time = Sys.time(), revision_id = NA)
    google_presentation$slide_size <- NA

    return(invisible())
  } else {
    rtn_env <- new.env()

    rtn_env$presentation_id <- id
    rtn_env$slide_ids <- NA
    rtn_env$current_slide_id <- NA
    rtn_env$version <- list(time = Sys.time(), revision_id = NA)
    rtn_env$slide_size <- NA
    return(rtn_env)
  }
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

    #' @field slides List of slide objects
    slides = NULL,

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
    #' Get slide object(s) from the presentation
    #'
    #' @param index Optional index/indices of specific slides to return
    #' @return List of slide objects, single slide object, or NULL
    get_slide = function(index = NULL) {
      if (is.null(self$slides) || length(self$slides) == 0) {
        cli::cli_alert_warning(
          "No slides available. Try calling $refresh() first."
        )
        return(NULL)
      }

      # Return all slides if no index specified
      # TODO: In the future, this should not be allowed. Error if missing index
      if (is.null(index)) {
        return(self$slides)
      }

      # Validate index
      # TODO: Also validate index is integer
      if (any(index < 1 | index > length(self$slides))) {
        cli::cli_abort(
          "Index out of bounds. Presentation has {length(self$slides)} slide{?s}."
        )
      }

      # Return single slide or list of slides
      # TODO: Validate and only return 1 slide at a time for Type stability. Consider a lower version similar to this and a higher level helper with type stability.
      if (length(index) == 1) {
        return(self$slides[[index]])
      } else {
        return(self$slides[index])
      }
    },

    #' @description
    #' Get slide IDs from the presentation
    #'
    #' @return Character vector of slide object IDs
    get_slide_ids = function() {
      if (is.null(self$slides) || length(self$slides) == 0) {
        return(character(0))
      }

      # TODO: Use purrr
      vapply(self$slides, function(slide) slide$objectId, character(1))
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

    # Create a new presentation via API
    create_new = function(title) {
      # Validate inputs
      if (!is.character(title) || length(title) != 1) {
        cli::cli_abort("{.arg title} must be a single string")
      }

      # Create the presentation
      create_body <- list(title = title)

      # Get rid of the FALSE when we have test_rsp
      rsp <- if (FALSE & is_testing()) {
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
      presentation_id <- private$resolve_id(id)

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

    # Resolve various ID formats to presentation_id
    resolve_id = function(id) {
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
    },

    # Populate object fields from API response
    populate_from_response = function(rsp) {
      # Take out when debugging is done
      assign("get_response", rsp, envir = .GlobalEnv)

      self$title <- rsp$title
      self$presentation_id <- rsp$presentationId
      self$revision_id <- rsp$revisionId
      self$locale <- rsp$locale
      self$page_size <- rsp$pageSize
      self$slides <- rsp$slides
      self$masters <- rsp$masters
      self$layouts <- rsp$layouts
      self$last_refreshed <- Sys.time()
    },

    # Set finalizer
    finalize = function() {
      if (self$is_active_flag) {
        self$set_not_active()
      }
    }
  )
)


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
    R6::is.R6(get("active_presentation", envir = .r2slides_objects))
  } else {
    FALSE
  }
}
