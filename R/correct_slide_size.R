#' Correct slide sizes
#'
#' @description
#' Google Slides uses a different default slide size than PowerPoint.
#' In order to make it easy for the user, and for re-usable code when working with both
#' Google Slides and PowerPoint, this function converts sizes and positions that use the
#' PowerPoint system to the Google Slides System. You may also override the slide_size argument
#' to convert between custom slide sizes.
#'
#' @param pos A length one numeric vector.
#' @param dim One of `"width"` or `"height"`.
#' @param slide_size Optional. A list containing slide size specifications for converting. In the form of list(x_height = 7.5, x_width = 13.3, y_height = 5, y_width = 9). `x_` indicates converting from and `y_` indicates converting too. You can override this to convert between custom slide sizes. If left blank it converts from PowerPoint to Google Slides.
#' @param call Optional. The execution environment, used for error messages.
#'
#' @returns
#' A numeric value of corrected position/size. Will error if `slide_size` is provided
#' but is not a list.
#'
#' @keywords internal
correct_slide_size <- function(
    pos,
    dim = c('width', 'height'),
    slide_size = NULL,
    call = rlang::caller_env()
) {
  dim <- rlang::arg_match(dim)

  if (!is.null(slide_size)) {
    if (!rlang::is_list(slide_size)) {
      cli::cli_abort("slide_size must be a list", call = call)
    }
  }

  if (dim == 'width') {
    slide_size$x_width <- slide_size$x_width %||% 13.33
    slide_size$y_width <- slide_size$y_width %||% 10

    pos * (slide_size$y_width / slide_size$x_width)
  } else if (dim == 'height') {
    slide_size$x_height <- slide_size$x_height %||% 7.5
    slide_size$y_height <- slide_size$y_height %||% 5.625

    pos * (slide_size$y_height / slide_size$x_height)
  }
}
