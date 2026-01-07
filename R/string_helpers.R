

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
#'
#' @return 
#' - `str_before()`: A numeric vector of length 2 containing the start position (1) and
#'   the position of the first occurrence of the character
#' - `str_after()`: A numeric vector of length 2 containing the position of the first
#'   occurrence of the character and the end of the string
#' - `str_matches()`: A matrix with start and end positions of the first match, or NA if
#'   no match is found
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
str_before <- function(text, character, include_boundary = FALSE) {
  if (include_boundary) {
    return(c(1, stringr::str_locate(text, stringr::coll(character))[[1]]))
  } else {
    return(c(1, stringr::str_locate(text, stringr::coll(character))[[1]] - 1))
  }
}

#' @rdname string-helpers
#' @export
str_after <- function(text, character, include_boundary = FALSE) {
  if (include_boundary) {
    return(c(stringr::str_locate(text, stringr::coll(character))[[1]], nchar(text)))
  } else {
    return(c(stringr::str_locate(text, stringr::coll(character))[[1]] + 1, nchar(text)))
  }
}

#' @rdname string-helpers
#' @export
str_matches <- function(text, pattern) {
  stringr::str_locate(text, pattern)
}