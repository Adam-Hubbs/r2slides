#' @import R6
NULL

#' R6 Class for Google Sheets Spreadsheets
#'
#' @description
#' An R6 class to represent a Google Sheets spreadsheet. Use [new_spreadsheet()]
#' or [register_spreadsheet()] to create instances.
#'
#' @export
spreadsheet <- R6::R6Class(
  "spreadsheet",
  cloneable = FALSE,
  public = list(
    #' @field spreadsheet_id The Google Sheets spreadsheet ID
    spreadsheet_id = NULL,

    #' @field title The title of the spreadsheet
    title = NULL,

    #' @field sheets A tibble of sheet names and IDs
    sheets = NULL,

    #' @description
    #' Create or open a spreadsheet
    #'
    #' @param id Spreadsheet ID, URL, or NULL to create new
    #' @param title Title for new spreadsheet (only used if id is NULL)
    #' @param set_active Whether to make this the active spreadsheet
    #'
    #' @return A new `spreadsheet` object
    initialize = function(
      id = NULL,
      title = "Untitled Spreadsheet",
      set_active = TRUE
    ) {
      if (is.null(id)) {
        private$create_new(title)
      } else {
        private$open_existing(id)
      }

      if (set_active) {
        if (active_spreadsheet_exists()) {
          if (is_testing()) {
            confirmation <- "y"
          } else {
            confirmation <- readline(
              "An active spreadsheet is already registered. Do you want to override it? (y/n): "
            )
          }
          if (tolower(confirmation) != "y") {
            cli::cli_alert_info(
              "Registration canceled, returning spreadsheet without setting as active."
            )
            return(invisible(self))
          }
        }
        self$set_active()
      }

      invisible(self)
    },

    #' @description
    #' Set this spreadsheet as the active one
    #'
    #' @return Self, invisibly
    set_active = function() {
      if (active_spreadsheet_exists()) {
        current <- get("active_spreadsheet", envir = .r2slides_objects)
        if (!identical(current, self)) {
          current$set_not_active()
        }
      }
      private$is_active_flag <- TRUE
      .r2slides_objects$active_spreadsheet <- self
      invisible(self)
    },

    #' @description
    #' Remove this spreadsheet as the active one
    #'
    #' @return Self, invisibly
    set_not_active = function() {
      private$is_active_flag <- FALSE
      assign("active_spreadsheet", NULL, envir = .r2slides_objects)
      invisible(self)
    },

    #' @description
    #' Check if this is the active spreadsheet
    #'
    #' @return Logical
    is_active = function() {
      private$is_active_flag
    },

    #' @description
    #' Get the Google Sheets URL for this spreadsheet
    #'
    #' @return Character URL or NULL
    get_url = function() {
      if (is.null(self$spreadsheet_id)) return(NULL)
      paste0("https://docs.google.com/spreadsheets/d/", self$spreadsheet_id)
    },

    #' @description
    #' Open the spreadsheet in a browser
    #'
    #' @return Self, invisibly
    browse = function() {
      url <- self$get_url()
      if (is.null(url)) {
        cli::cli_abort("Cannot browse: {.field spreadsheet_id} is not set")
      }
      utils::browseURL(url)
      invisible(self)
    },

    #' @description
    #' Print method for spreadsheet objects
    #'
    #' @param ... Additional arguments (unused)
    print = function(...) {
      cli::cli_h2("Google Sheets Spreadsheet")
      cli::cli_dl(c(
        "Title" = self$title %||% "{.emph Not set}",
        "ID" = self$spreadsheet_id %||% "{.emph Not set}",
        "Sheets" = as.character(
          if (!is.null(self$sheets)) nrow(self$sheets) else 0L
        ),
        "Active" = if (private$is_active_flag) "{.strong Yes}" else "No"
      ))
      if (!is.null(self$spreadsheet_id)) {
        cli::cli_text("{.url {self$get_url()}}")
      }
      invisible(self)
    }
  ),

  private = list(
    is_active_flag = FALSE,

    create_new = function(title) {
      ss_id <- googlesheets4::gs4_create(name = title)
      meta <- googlesheets4::gs4_get(ss_id)
      private$populate(meta)
      cli::cli_alert_success("Created spreadsheet {.val {self$title}}")
      invisible(self)
    },

    open_existing = function(id) {
      ss_id <- tryCatch(
        googlesheets4::as_sheets_id(id),
        error = \(e)
          cli::cli_abort(
            "Could not resolve {.arg id} to a spreadsheet ID.",
            parent = e
          )
      )
      meta <- tryCatch(
        googlesheets4::gs4_get(ss_id),
        error = \(e)
          cli::cli_abort(
            "Could not access spreadsheet. Check the ID is valid and you have permission.",
            parent = e
          )
      )
      private$populate(meta)
      cli::cli_alert_success("Opened spreadsheet {.val {self$title}}")
      invisible(self)
    },

    populate = function(meta) {
      self$spreadsheet_id <- meta$spreadsheet_id
      self$title <- meta$name
      self$sheets <- dplyr::select(meta$sheets, "name", "id")
    },

    finalize = function() {
      if (private$is_active_flag) {
        self$set_not_active()
      }
    }
  )
)


#' Create a new Google Sheets spreadsheet
#'
#' @param title Optional. A single string. The title of the spreadsheet.
#' @param set_active Optional. A logical value indicating whether to set the
#'   spreadsheet as the active spreadsheet. Default: `TRUE`.
#'
#' @returns A `spreadsheet` object
#' @export
new_spreadsheet <- function(title = "Untitled Spreadsheet", set_active = TRUE) {
  spreadsheet$new(title = title, set_active = set_active)
}


#' Register an existing Google Sheets spreadsheet
#'
#' @description
#' Registers an existing Google Sheets spreadsheet so you can use it in r2slides.
#' Supply a spreadsheet ID string or a Google Sheets URL.
#'
#' @param id A spreadsheet ID string or Google Sheets URL.
#' @param set_active Optional. A logical value indicating whether to set the
#'   spreadsheet as the active spreadsheet. Default: `TRUE`.
#'
#' @returns A `spreadsheet` object
#' @export
register_spreadsheet <- function(id, set_active = TRUE) {
  spreadsheet$new(id = id, set_active = set_active)
}


#' Get the currently active spreadsheet
#'
#' @return A `spreadsheet` object, or errors if none is active
#' @export
get_active_spreadsheet <- function() {
  if (active_spreadsheet_exists()) {
    get("active_spreadsheet", envir = .r2slides_objects)
  } else {
    cli::cli_abort(
      "No active spreadsheet. Use {.fn register_spreadsheet} or {.fn new_spreadsheet} first."
    )
  }
}

active_spreadsheet_exists <- function() {
  exists("active_spreadsheet", envir = .r2slides_objects) &&
    is.spreadsheet(get("active_spreadsheet", envir = .r2slides_objects))
}


#' Check if an object is a spreadsheet
#'
#' @param x An object to check
#'
#' @return `TRUE` if `x` is a `spreadsheet`, `FALSE` otherwise
#'
#' @export
is.spreadsheet <- function(x) {
  inherits(x, "spreadsheet")
}

#' @exportS3Method googlesheets4::as_sheets_id
as_sheets_id.spreadsheet <- function(x, ...) {
  googlesheets4::as_sheets_id(x$spreadsheet_id)
}
