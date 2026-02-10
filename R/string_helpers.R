#' String helper functions for text positioning
#'
#' Helper functions to extract position information for text styling and
#' pattern matching. These functions return position vectors that can be used
#' with styling rules to format specific parts of text strings.
#'
#' @param text A character string to search in
#' @param character A character string to search for (literal match)
#' @param include_boundary A logical indicating whether to include the position of the boundary character
#' @param pattern A regular expression pattern to search for
#' @param missing A character string indicating how to handle missing positions. Options are "error", "none", or "all".
#' @return
#' - `str_before()`: A numeric vector of length 2 containing the start position (1) and
#'   the position of the first occurrence of the character
#' - `str_after()`: A numeric vector of length 2 containing the position of the first
#'   occurrence of the character and the end of the string
#' - `str_matches()`: A matrix with start and end positions of the first match
#'
#' @examples
#' # Extract positions before and after a dot
#' str_before("hello.world", ".")
#' str_after("hello.world", ".")
#'
#' # Find pattern matches
#' str_matches("hello.world", "ello")
#' str_matches("test123", "\\d+")
#'
#' @name string-helpers
#' @rdname string-helpers
#' @export
str_before <- function(
  text,
  character,
  include_boundary = FALSE,
  missing = c('error', 'none', 'all')
) {
  missing <- rlang::arg_match(missing)
  location <- stringr::str_locate(text, stringr::coll(character))

  if (is.null(text) | is.null(character)) {
    tmp <- NA
  } else if (include_boundary) {
    tmp <- c(1, location[[1, 2]])
  } else {
    tmp <- c(1, location[[1, 1]] - 1)
  }
  if (anyNA(tmp)) {
    if (missing == 'error') {
      cli::cli_abort(
        "Character {.val {character}} not found in text {.val {text}}"
      )
    } else if (missing == 'none') {
      tmp <- c(0, 0)
    } else if (missing == 'all') {
      tmp <- c(1, nchar(text))
    }
  }
  return(tmp)
}

#' @rdname string-helpers
#' @export
str_after <- function(
  text,
  character,
  include_boundary = FALSE,
  missing = c('error', 'none', 'all')
) {
  missing <- rlang::arg_match(missing)
  location <- stringr::str_locate(text, stringr::coll(character))

  if (is.null(text) | is.null(character)) {
    tmp <- NA
  } else if (include_boundary) {
    tmp <- c(location[[1, 1]], nchar(text))
  } else {
    tmp <- c(location[[1, 2]] + 1, nchar(text))
  }

  if (anyNA(tmp)) {
    if (missing == 'error') {
      cli::cli_abort(
        "Character {.val {character}} not found in text {.val {text}}"
      )
    } else if (missing == 'none') {
      tmp <- c(0, 0)
    } else if (missing == 'all') {
      tmp <- c(1, nchar(text))
    }
  }
  return(tmp)
}

#' @rdname string-helpers
#' @export
str_matches <- function(text, pattern, missing = c('error', 'none', 'all')) {
  missing <- rlang::arg_match(missing)

  location <- stringr::str_locate(text, pattern)
  if (is.null(text) | is.null(pattern)) {
    tmp <- NA
  } else {
    tmp <- c(location[[1, 1]], location[[1, 2]])
  }

  if (anyNA(tmp)) {
    if (missing == 'error') {
      cli::cli_abort(
        "Pattern {{pattern} .val} not found in text {{text} .val}"
      )
    } else if (missing == 'none') {
      tmp <- c(0, 0)
    } else if (missing == 'all') {
      tmp <- c(1, nchar(text))
    }
  }
  return(tmp)
}
