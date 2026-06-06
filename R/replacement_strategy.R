#' Replacement strategy parameters
#'
#' @param replacement_strategy One of `"add"` (default), `"replace"`, or
#'   `"skip"`. Overrides the package-wide default set by
#'   [set_replacement_strategy()]. `"add"` always creates a new element.
#'   `"replace"` deletes any matching element and creates a fresh one. `"skip"`
#'   leaves a matching element untouched and suppresses creation.
#' @param match_fn A function that identifies an existing slide element matching
#'   the new one. Must have signature
#'   `function(new_spec, existing_elements) -> character(1) | NULL`. See
#'   [match_by_type_and_position()] for details and the built-in implementation.
#'   Overrides the package-wide default set by [set_match_fn()].
#' @name replacement_strategy_params
#' @keywords internal
NULL

#' Get page elements for a slide
#'
#' Returns a tibble of all page elements on a slide, with columns
#' `element_id`, `type`, `left`, `top`, `width`, and `height` (dimensions in
#' inches). Useful for inspecting slide content and writing custom match
#' functions.
#'
#' `type` is one of `"TEXT_BOX"`, `"SHEETS_CHART"`, `"IMAGE"`, `"TABLE"`, or
#' `"UNSUPPORTED"`.
#'
#' @param slide A `r2slides::slide` object.
#' @param ps The presentation object owning the slide. Defaults to
#'   `slide@presentation`.
#'
#' @return A [tibble::tibble()] with columns `element_id`, `type`, `left`,
#'   `top`, `width`, `height`.
#' @export
get_page_elements <- function(slide, ps = slide@presentation) {
  rsp <- query(
    endpoint = "slides.presentations.pages.get",
    params = list(
      presentationId = ps$presentation_id,
      pageObjectId = slide@slide_id
    ),
    base = "slides"
  )

  elements <- rsp$pageElements %||% list()

  empty <- tibble::tibble(
    element_id = character(),
    type = character(),
    left = numeric(),
    top = numeric(),
    width = numeric(),
    height = numeric()
  )

  if (length(elements) == 0L) return(empty)

  purrr::map(elements, .normalize_page_element) |> purrr::list_rbind()
}

# Returns the r2slides type string for a raw pageElement list.
detect_element_type <- function(el) {
  if (!is.null(el$shape) && identical(el$shape$shapeType, "TEXT_BOX")) {
    return("TEXT_BOX")
  }
  if (!is.null(el$sheetsChart)) return("SHEETS_CHART")
  if (!is.null(el$image)) return("IMAGE")
  if (!is.null(el$table)) return("TABLE")
  "UNSUPPORTED"
}

# Converts one raw pageElement list into a single-row tibble.
# Width and height multiply size by scaleX/scaleY because Google Slides
# normalizes stored transforms: the same rendered size may be represented as
# (size=3000000, scaleX=0.9754) rather than (size=2926080, scaleX=1).
.normalize_page_element <- function(el) {
  emu <- 914400L
  tibble::tibble_row(
    element_id = el$objectId %||% NA_character_,
    type = detect_element_type(el),
    left = (el$transform$translateX %||% 0) / emu,
    top = (el$transform$translateY %||% 0) / emu,
    width = (el$size$width$magnitude %||% 0) *
      (el$transform$scaleX %||% 1) /
      emu,
    height = (el$size$height$magnitude %||% 0) *
      (el$transform$scaleY %||% 1) /
      emu
  )
}

#' Match elements by type and position
#'
#' Returns a match function that finds an existing slide element with the same
#' type whose centroid falls within `tolerance` inches of the new element's
#' centroid. If multiple elements match, a warning is issued and `NULL` is
#' returned (no replacement is made).
#'
#' @param tolerance Numeric. Maximum distance in inches between centroids for a
#'   match to be accepted. Default: `0.05`.
#'
#' @return A function with signature
#'   `function(new_spec, existing_elements) -> character(1) | NULL`.
#'   `new_spec` is a named list with `type`, `left`, `top`, `width`, `height`.
#'   `existing_elements` is a tibble with those same columns plus `element_id`.
#' @seealso [set_match_fn()], [get_match_fn()], [set_replacement_strategy()]
#' @importFrom rlang .data
#' @export
match_by_type_and_position <- function(tolerance = 0.05) {
  function(new_spec, existing_elements) {
    if (nrow(existing_elements) == 0L) return(NULL)

    type_matches <- dplyr::filter(
      existing_elements,
      .data$type == new_spec$type
    )
    if (nrow(type_matches) == 0L) return(NULL)

    new_cx <- new_spec$left + new_spec$width / 2
    new_cy <- new_spec$top + new_spec$height / 2

    pos_matches <- dplyr::filter(
      type_matches,
      abs((.data$left + .data$width / 2) - new_cx) <= tolerance,
      abs((.data$top + .data$height / 2) - new_cy) <= tolerance
    )

    if (nrow(pos_matches) == 0L) return(NULL)

    if (nrow(pos_matches) > 1L) {
      cli::cli_warn(c(
        "!" = "{nrow(pos_matches)} existing {.val {new_spec$type}} elements match the new element.",
        "i" = "No replacement will be made. Reduce {.arg tolerance} or write a custom match function."
      ))
      return(NULL)
    }

    pos_matches$element_id
  }
}

#' Set the replacement strategy
#'
#' Controls how `add_*` functions behave when an existing element on the slide
#' matches the new one (as determined by the active match function).
#'
#' - `"add"` (default): always create a new element; match function is never called.
#' - `"replace"`: delete the matched element, then create a fresh one. If no
#'   match is found, a new element is created.
#' - `"skip"`: leave the matched element untouched and skip creation. If no
#'   match is found, a new element is created.
#'
#' @param strategy One of `"add"`, `"replace"`, or `"skip"`.
#'
#' @return The strategy string, invisibly.
#' @seealso [get_replacement_strategy()], [set_match_fn()],
#'   [match_by_type_and_position()]
#' @export
set_replacement_strategy <- function(strategy = c("add", "replace", "skip")) {
  strategy <- rlang::arg_match(strategy)
  Sys.setenv(r2slides_replacement_strategy = strategy)
  invisible(strategy)
}

#' Get the current replacement strategy
#'
#' Returns the value of the `r2slides_replacement_strategy` environment
#' variable, defaulting to `"add"` if unset.
#'
#' @return A string: `"add"`, `"replace"`, or `"skip"`.
#' @seealso [set_replacement_strategy()]
#' @export
get_replacement_strategy <- function() {
  strategy <- Sys.getenv("r2slides_replacement_strategy", unset = "add")
  if (!strategy %in% c("add", "replace", "skip")) {
    cli::cli_warn(c(
      "!" = "Unknown replacement strategy {.val {strategy}}; defaulting to {.val add}.",
      "i" = "Use {.fn set_replacement_strategy} to set a valid strategy."
    ))
    return("add")
  }
  strategy
}

#' Set the match function
#'
#' Sets the package-wide function used to find an existing slide element that
#' matches the one being added. Used by `add_*` functions when the replacement
#' strategy is `"replace"` or `"skip"`.
#'
#' The function must accept two arguments:
#' - `new_spec`: a named list with `type` (character), `left`, `top`, `width`,
#'   `height` (numeric, in inches).
#' - `existing_elements`: a tibble with the same columns plus `element_id`.
#'
#' It must return either a single `element_id` string or `NULL`. If multiple
#' elements match, the function should warn and return `NULL`.
#'
#' @param fn A function with the signature described above.
#'
#' @return `fn`, invisibly.
#' @seealso [get_match_fn()], [match_by_type_and_position()]
#' @export
set_match_fn <- function(fn) {
  if (!is.function(fn)) {
    cli::cli_abort("{.arg fn} must be a function.")
  }
  .r2slides_objects$match_fn <- fn
  invisible(fn)
}

#' Get the current match function
#'
#' Returns the package-wide match function set by [set_match_fn()]. The default
#' is [match_by_type_and_position()] with `tolerance = 0.05`.
#'
#' @return A function.
#' @seealso [set_match_fn()], [match_by_type_and_position()]
#' @export
get_match_fn <- function() {
  .r2slides_objects$match_fn
}

# Build an element spec from an element type and a slide position.
element_spec <- function(element_type, position) {
  list(
    type = element_type,
    left = position@left,
    top = position@top,
    width = position@width,
    height = position@height
  )
}

# Applies the replacement strategy before one or more add_* calls.
#
# `positions` may be a single slide_position or a list of them.
# `element_ids` may be NULL (single call) or a list of character(1)|NULL.
# Elements with an explicit element_id are updates and always proceed.
#
# Returns a scalar logical for a single position, a logical vector for a list.
# When strategy is "replace", matched elements are deleted in one batch request.
# get_page_elements() is called at most once regardless of how many positions
# are checked.
apply_replacement <- function(
  slide_obj,
  element_type,
  positions,
  element_ids = NULL,
  strategy,
  match_fn,
  debug = FALSE
) {
  scalar_call <- !is.list(positions)
  if (scalar_call) positions <- list(positions)

  n <- length(positions)

  if (is.null(element_ids)) element_ids <- rep(list(NULL), n)

  needs_check <- purrr::map_lgl(element_ids, is.null)

  if (!any(needs_check) || strategy == "add") {
    result <- rep(TRUE, n)
    return(if (scalar_call) result[[1L]] else result)
  }

  existing <- get_page_elements(slide_obj)
  check_idx <- which(needs_check)

  matched <- purrr::map_chr(
    positions[check_idx],
    \(pos) {
      match_fn(element_spec(element_type, pos), existing) %||% NA_character_
    }
  )

  proceed <- rep(TRUE, n)

  if (strategy == "skip") {
    has_match <- !is.na(matched)
    proceed[check_idx] <- !has_match
    if (any(has_match)) {
      n_skip <- sum(has_match)
      cli::cli_inform(c(
        "i" = "Skipping {n_skip} {.val {element_type}} element{?s}: matching element{?s} {?already exists/already exist} at {?this position/these positions}."
      ))
    }
    return(if (scalar_call) proceed[[1L]] else proceed)
  }

  # strategy == "replace"
  delete_ids <- matched[!is.na(matched)]
  if (length(delete_ids) > 0L && !debug) {
    purrr::walk(
      delete_ids,
      \(id) delete_element(slide_obj@presentation$presentation_id, id)
    )
  }

  if (scalar_call) proceed[[1L]] else proceed
}

# Delete a single page element via batchUpdate deleteObject.
delete_element <- function(presentation_id, element_id) {
  query(
    endpoint = "slides.presentations.batchUpdate",
    params = list(presentationId = presentation_id),
    body = list(
      requests = list(list(
        deleteObject = list(objectId = element_id)
      ))
    ),
    base = "slides"
  )
  invisible(NULL)
}
