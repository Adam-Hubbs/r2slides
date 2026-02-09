#' @import S7
NULL

#' Slide Position Object
#'
#' A system for working with Google Slides positions and sizes.
#'
#' @param top The position in inches from the top of the slide
#' @param left The position in inches from the left of the slide
#' @param width The width of the object in inches
#' @param height The height of the object in inches
#' @param convert_slide_size A logical. Optional. Converts the position and size to Google Slides from a PowerPoint (or custom) size if TRUE.
#' @param slide_size_old Old Slide size specifications. Optional. Used for conversion between powerpoint and google slides sizes.
#' @param slide_size New Slide size specifications. Optional.
#'
#' @export
slide_position <- S7::new_class(
  "slide_position",
  properties = list(
    top = S7::new_property(S7::class_double, validator = function(value) {
      if (length(value) != 1) {
        return("top must be a single value")
      }
      if (value < 0) return("top must be greater than or equal to 0")
    }),
    left = S7::new_property(S7::class_double, validator = function(value) {
      if (length(value) != 1) {
        return("left must be a single value")
      }
      if (value < 0) return("left must be greater than or equal to 0")
    }),
    width = S7::new_property(S7::class_double, validator = function(value) {
      if (length(value) != 1) {
        return("width must be a single value")
      }
      if (value <= 0) return("width must be greater than 0")
    }),
    height = S7::new_property(S7::class_double, validator = function(value) {
      if (length(value) != 1) {
        return("height must be a single value")
      }
      if (value <= 0) return("height must be greater than 0")
    }),

    # Defaults for slide size
    slide_width = S7::new_property(
      S7::class_double,
      default = 10,
      validator = function(value) {
        if (length(value) != 1) {
          return("slide_width must be a single value")
        }
        if (value <= 0) return("slide_width must be greater than 0")
      }
    ),
    slide_height = S7::new_property(
      S7::class_double,
      default = 5.625,
      validator = function(value) {
        if (length(value) != 1) {
          return("slide_height must be a single value")
        }
        if (value <= 0) return("slide_height must be greater than 0")
      }
    ),

    # Computed properties
    top_end = S7::new_property(S7::class_double, getter = function(self) {
      self@top + self@height
    }),
    left_end = S7::new_property(S7::class_double, getter = function(self) {
      self@left + self@width
    }),
    top_pt = S7::new_property(S7::class_double, getter = function(self) {
      self@top * 72
    }),
    left_pt = S7::new_property(S7::class_double, getter = function(self) {
      self@left * 72
    }),
    width_pt = S7::new_property(S7::class_double, getter = function(self) {
      self@width * 72
    }),
    height_pt = S7::new_property(S7::class_double, getter = function(self) {
      self@height * 72
    })
  ),
  constructor = function(
    top,
    left,
    width,
    height,
    convert_slide_size = FALSE,
    slide_size_old = NULL,
    slide_size = c(5.625, 10)
  ) {
    if (convert_slide_size) {
      if (is.null(slide_size_old)) {
        cli::cli_abort(c(
          "slide_size_old must be provided when convert_slide_size is TRUE",
          "i" = "Provide a numeric vector of length 2: c(height, width)"
        ))
      }
      if (is.null(slide_size)) {
        cli::cli_abort(c(
          "slide_size must be provided when convert_slide_size is TRUE",
          "i" = "Provide a numeric vector of length 2: c(height, width)"
        ))
      }
      if (!is.numeric(slide_size_old) || length(slide_size_old) != 2) {
        cli::cli_abort(c(
          "slide_size_old must be a numeric vector of length 2",
          "x" = "Got {.type {slide_size_old}} of length {length(slide_size_old)}",
          "i" = "Expected format: c(height, width)"
        ))
      }
      if (!is.numeric(slide_size) || length(slide_size) != 2) {
        cli::cli_abort(c(
          "slide_size must be a numeric vector of length 2",
          "x" = "Got {.type {slide_size}} of length {length(slide_size)}",
          "i" = "Expected format: c(height, width)"
        ))
      }
      slide_size <- list(
        x_height = slide_size_old[1],
        x_width = slide_size_old[2],
        y_height = slide_size[1],
        y_width = slide_size[2]
      )
      left <- correct_slide_size(left, dim = 'width', slide_size)
      top <- correct_slide_size(top, dim = 'height', slide_size)
      width <- correct_slide_size(width, dim = 'width', slide_size)
      height <- correct_slide_size(height, dim = 'height', slide_size)
    }

    if (!is.null(slide_size)) {
      if (length(slide_size) != 2) {
        cli::cli_abort("slide_size must be a numeric vector of length 2")
      }

      new_object(
        S7_object(),
        top = top,
        left = left,
        width = width,
        height = height,
        slide_height = slide_size[1],
        slide_width = slide_size[2]
      )
    } else {
      new_object(
        S7_object(),
        top = top,
        left = left,
        width = width,
        height = height
      )
    }
  }
)


# Register print method
S7::method(print, slide_position) <- function(x, ...) {
  cli::cli_div(
    theme = list(span.val = list(color = "blue", "font-weight" = "bold"))
  )

  cli::cli_text("{.strong Slide Position Object}")
  cli::cli_text("Position:")
  cli::cli_bullets(
    c(
      "*" = "From Left: {.val {x@left}}",
      "*" = "From Top: {.val {x@top}}"
    )
  )
  cli::cli_text("Size:")
  cli::cli_bullets(
    c(
      "*" = "Width: {.val {x@width}}",
      "*" = "Height: {.val {x@height}}"
    )
  )
  cli::cli_text("Left bounds: {.val {x@left}} -> {.val {x@left_end}}")
  cli::cli_text("Top bounds: {.val {x@top}} -> {.val {x@top_end}}")
  cat("\n")
  cli::cli_text("Run {.code plot()} to plot the slide position.")

  invisible(x)
}


# Register plot method
S7::method(plot, slide_position) <- function(
  x,
  y = NULL,
  ...,
  slide_size = c(5.625, 10)
) {
  # Extract slide dimensions (height, width)
  slide_height <- slide_size[1]
  slide_width <- slide_size[2]

  # Set up plot margins to accommodate rulers and annotations
  par(mar = c(2, 2, 2, 2))

  # Create the main plot area - flip y-axis so top=0 is at the top
  plot(
    0,
    0,
    type = "n",
    xlim = c(-1, slide_width + 1),
    ylim = c(slide_height + 1, -1), # Flipped y-axis
    xlab = "",
    ylab = "",
    main = "",
    axes = FALSE,
    asp = 1
  )

  # Draw the slide boundary rectangle
  rect(
    0,
    0,
    slide_width,
    slide_height,
    col = "white",
    border = "black",
    lwd = 2
  )

  # Add "SLIDE" text in background
    text(
      slide_width / 2,
      slide_height / 2,
      "SLIDE",
      cex = 6,
      srt = tan(slide_height / slide_width) * 180 / pi,
      col = "grey",
      font = 2, # Bold
      adj = c(0.5, 0.5)
    )
  
  

  # Draw the position rectangle
  rect(
    x@left,
    x@top,
    x@left_end,
    x@top_end,
    col = "lightblue",
    border = "darkblue",
    lwd = 2
  )

  # Add ruler marks on top (x-axis)
  ruler_ticks <- seq(0, slide_width, by = 0.5)
  for (tick in ruler_ticks) {
    lines(c(tick, tick), c(-0.1, -0.2), col = "black")
    if (tick %% 1 == 0) {
      # Major ticks at whole numbers
      lines(c(tick, tick), c(-0.1, -0.3), col = "black", lwd = 2)
      text(tick, -0.5, tick, cex = 0.7, col = "black")
    }
  }

  # Add ruler marks on left (y-axis)
  ruler_ticks_y <- seq(0, slide_height, by = 0.5)
  for (tick in ruler_ticks_y) {
    lines(c(-0.1, -0.2), c(tick, tick), col = "black")
    if (tick %% 1 == 0) {
      # Major ticks at whole numbers
      lines(c(-0.1, -0.3), c(tick, tick), col = "black", lwd = 2)
      text(-0.5, tick, tick, cex = 0.7, col = "black", srt = 90)
    }
  }

  # Add dimension annotations - just values, no labels - ALL BLUE
  # Left position line
  lines(
    c(x@left, x@left),
    c(slide_height + 0.15, slide_height + 0.35),
    col = "blue",
    lwd = 1.5
  )
  text(
    x@left,
    slide_height + 0.6,
    glue::glue("{x@left}"),
    cex = 0.8,
    col = "blue",
    srt = 90,
    adj = c(1, 0.5)
  )

  # Left end position line
  lines(
    c(x@left_end, x@left_end),
    c(slide_height + 0.15, slide_height + 0.35),
    col = "blue",
    lwd = 1.5
  )
  text(
    x@left_end,
    slide_height + 0.6,
    glue::glue("{x@left_end}"),
    cex = 0.8,
    col = "blue",
    srt = 90,
    adj = c(1, 0.5)
  )

  # Width annotation
  arrows(
    x@left,
    slide_height + 0.25,
    x@left_end,
    slide_height + 0.25,
    code = 3,
    angle = 20,
    length = 0.1,
    col = "blue",
    lwd = 1.5
  )
  text(
    (x@left + x@left_end) / 2,
    slide_height + 0.35,
    glue::glue("{x@width}"),
    cex = 0.8,
    col = "blue",
    adj = c(0.5, 1)
  )

  # Top position line - moved further left
  lines(c(-0.6, -0.8), c(x@top, x@top), col = "blue", lwd = 1.5)
  text(
    -0.9, # Moved further left
    x@top,
    glue::glue("{x@top}"),
    cex = 0.8,
    col = "blue",
    srt = 0,
    adj = c(1, 0.5)
  )

  # Top end position line - moved further left
  lines(c(-0.6, -0.8), c(x@top_end, x@top_end), col = "blue", lwd = 1.5)
  text(
    -0.9, # Moved further left
    x@top_end,
    glue::glue("{x@top_end}"),
    cex = 0.8,
    col = "blue",
    srt = 0,
    adj = c(1, 0.5)
  )

  # Height annotation - moved further left
  arrows(
    -0.7, # Moved further left
    x@top,
    -0.7, # Moved further left
    x@top_end,
    code = 3,
    angle = 20,
    length = 0.1,
    col = "blue",
    lwd = 1.5
  )
  text(
    -0.8, # Moved further left
    (x@top + x@top_end) / 2,
    glue::glue("{x@height}"),
    cex = 0.8,
    col = "blue",
    srt = 90,
    adj = c(0.5, 0)
  )

  invisible(x)
}

S7::method(convert, list(slide_position, S7::class_list)) <- function(
  from,
  to
) {
  list(
    left = from@left,
    top = from@top,
    width = from@width,
    height = from@height
  )
}

S7::method(`+`, list(slide_position, S7::class_numeric)) <- function(e1, e2) {
  slide_position(
    top = e1@top,
    left = e1@left,
    width = e1@width + e2,
    height = e1@height + e2
  )
}


S7::method(`+`, list(slide_position, slide_position)) <- function(e1, e2) {
  # Check for incompatible slide sizes
  if (e1@slide_width != e2@slide_width || e1@slide_height != e2@slide_height) {
    cli::cli_abort(c(
      x = "Cannot add slide position objects",
      i = "Slide position objects have incompatible slide sizes",
      i = "Slide size 1: {e1@slide_width} x {e1@slide_height}",
      i = "Slide size 2: {e2@slide_width} x {e2@slide_height}"
    ))
  }
  slide_position(
    top = min(e1@top, e2@top),
    left = min(e1@left, e2@left),
    width = max(e1@left_end, e2@left_end) - min(e1@left, e2@left),
    height = max(e1@top_end, e2@top_end) - min(e1@top, e2@top),
    slide_size = c(e1@slide_height, e1@slide_width)
  )
}

mirror <- new_generic('mirror', 'x', function(x, ...) {
  S7_dispatch()
})

S7::method(mirror, slide_position) <- function(
  x,
  flip_axis = c('Horizontal', 'Vertical')
) {
  flip_axis <- rlang::arg_match(flip_axis)

  if (flip_axis == 'Horizontal') {
    slide_position(
      top = x@top,
      left = x@slide_width - x@left - x@width,
      width = x@width,
      height = x@height,
      slide_size = c(x@slide_height, x@slide_width)
    )
  } else if (flip_axis == 'Vertical') {
    slide_position(
      top = x@slide_height - x@top - x@height,
      left = x@left,
      width = x@width,
      height = x@height,
      slide_size = c(x@slide_height, x@slide_width)
    )
  }
}


#' Creates a slide_position object with defaults in the top left of the slide
#' @param convert_slide_size A logical. Optional.
#' @param slide_size_old Old Slide size specifications. Optional. Used for conversion between powerpoint and google slides sizes
#' @param slide_size New Slide size specifications. Optional.
#'
#' @export
in_top_left <- function(
  convert_slide_size = FALSE,
  slide_size_old = NULL,
  slide_size = c(5.625, 10)
) {
  slide_position(
    top = 1.48,
    left = 0.12,
    width = 3.2,
    height = 1.8,
    convert_slide_size = convert_slide_size,
    slide_size_old = slide_size_old,
    slide_size = slide_size
  )
}


#' Creates a slide_position object with defaults in the top middle of the slide
#' @param convert_slide_size A logical. Optional.
#' @param slide_size_old Old Slide size specifications. Optional. Used for conversion between powerpoint and google slides sizes
#' @param slide_size New Slide size specifications. Optional.
#'
#' @export
in_top_middle <- function(
  convert_slide_size = FALSE,
  slide_size_old = NULL,
  slide_size = c(5.625, 10)
) {
  slide_position(
    top = 1.48,
    left = 3.42,
    width = 3.2,
    height = 1.8,
    convert_slide_size = convert_slide_size,
    slide_size_old = slide_size_old,
    slide_size = slide_size
  )
}


#' Creates a slide_position object with defaults in the top right of the slide
#' @param convert_slide_size A logical. Optional.
#' @param slide_size_old Old Slide size specifications. Optional. Used for conversion between powerpoint and google slides sizes
#' @param slide_size New Slide size specifications. Optional.
#'
#' @export
in_top_right <- function(
  convert_slide_size = FALSE,
  slide_size_old = NULL,
  slide_size = c(5.625, 10)
) {
  slide_position(
    top = 1.48,
    left = 6.72,
    width = 3.2,
    height = 1.8,
    convert_slide_size = convert_slide_size,
    slide_size_old = slide_size_old,
    slide_size = slide_size
  )
}

#' Creates a slide_position object with defaults in the bottom left of the slide
#' @param convert_slide_size A logical. Optional.
#' @param slide_size_old Old Slide size specifications. Optional. Used for conversion between powerpoint and google slides sizes
#' @param slide_size New Slide size specifications. Optional.
#'
#' @export
in_bottom_left <- function(
  convert_slide_size = FALSE,
  slide_size_old = NULL,
  slide_size = c(5.625, 10)
) {
  slide_position(
    top = 3.69,
    left = 0.12,
    width = 3.2,
    height = 1.8,
    convert_slide_size = convert_slide_size,
    slide_size_old = slide_size_old,
    slide_size = slide_size
  )
}


#' Creates a slide_position object with defaults in the bottom middle of the slide
#' @param convert_slide_size A logical. Optional.
#' @param slide_size_old Old Slide size specifications. Optional. Used for conversion between powerpoint and google slides sizes
#' @param slide_size New Slide size specifications. Optional.
#'
#' @export
in_bottom_middle <- function(
  convert_slide_size = FALSE,
  slide_size_old = NULL,
  slide_size = c(5.625, 10)
) {
  slide_position(
    top = 3.69,
    left = 3.42,
    width = 3.2,
    height = 1.8,
    convert_slide_size = convert_slide_size,
    slide_size_old = slide_size_old,
    slide_size = slide_size
  )
}


#' Creates a slide_position object with defaults in the bottom right of the slide
#' @param convert_slide_size A logical. Optional.
#' @param slide_size_old Old Slide size specifications. Optional. Used for conversion between powerpoint and google slides sizes
#' @param slide_size New Slide size specifications. Optional.
#'
#' @export
in_bottom_right <- function(
  convert_slide_size = FALSE,
  slide_size_old = NULL,
  slide_size = c(5.625, 10)
) {
  slide_position(
    top = 3.69,
    left = 6.72,
    width = 3.2,
    height = 1.8,
    convert_slide_size = convert_slide_size,
    slide_size_old = slide_size_old,
    slide_size = slide_size
  )
}

#' Creates a slide_position object with defaults in the title of the slide
#' @param revision A date object to specify the defaults for qualtrics title as of a given date
#' @param convert_slide_size A logical. Optional.
#' @param slide_size_old Old Slide size specifications. Optional. Used for conversion between powerpoint and google slides sizes
#' @param slide_size New Slide size specifications. Optional.
#'
#' @export
in_qualtrics_title <- function(
  revision = Sys.Date(),
  convert_slide_size = FALSE,
  slide_size_old = NULL,
  slide_size = c(5.625, 10)
) {
  slide_position(
    top = 0.25,
    left = 0.5,
    width = 10.5,
    height = 0.75,
    convert_slide_size = convert_slide_size,
    slide_size_old = slide_size_old,
    slide_size = slide_size
  )
}


#' Transform a slide position object to create a new position
#' @noRd
process_transformation <- function(
  trans,
  param_name,
  call = rlang::caller_env()
) {
  # If it's a scalar numeric, convert to a function that returns that value
  if (is.numeric(trans) && length(trans) == 1) {
    return(function(x) trans)
  }

  # If it's a function, return as-is
  if (is.function(trans)) {
    return(trans)
  }

  # Otherwise, error
  cli::cli_abort(
    "{.arg {param_name}} must be either a function or a scalar numeric value",
    call = call
  )
}

#' Relative Position
#'
#' Takes a slide position object, applies transformations to its dimensions,
#' and returns a new slide_position object. Transformations can be either
#' functions or scalar numeric values. If a scalar is provided, it replaces
#' the original value. If a function is provided, it's applied to the original
#' value.
#'
#' @param position An object of class `r2slides::slide_position`
#' @param top_transformation A function to apply to the top value, or a scalar numeric to replace it
#' @param left_transformation A function to apply to the left value, or a scalar numeric to replace it
#' @param width_transformation A function to apply to the width value, or a scalar numeric to replace it
#' @param height_transformation A function to apply to the height value, or a scalar numeric to replace it
#'
#' @returns A new object of class `r2slides::slide_position`
#'
#' @examples
#' \dontrun{
#' # Create a base position
#' base_pos <- slide_position(left = 1, top = 2, width = 5, height = 3)
#'
#' # Move right by 0.5 inches using a function, set the maximum width at 5
#' new_pos <- relative_position(
#'   base_pos,
#'   top_transformation = identity,
#'   left_transformation = function(x) x + 0.5,
#'   width_transformation = \(x) min(x, 5),
#'   height_transformation = identity
#' )
#'
#' # Set absolute position using scalars
#' new_pos2 <- relative_position(
#'   base_pos,
#'   top_transformation = 3,
#'   left_transformation = 2,
#'   width_transformation = identity,
#'   height_transformation = identity
#' )
#' }
#'
#' @export
relative_position <- function(
  position,
  top_transformation = identity,
  left_transformation = identity,
  width_transformation = identity,
  height_transformation = identity
) {
  # Validate input position
  if (!inherits(position, "r2slides::slide_position")) {
    cli::cli_abort(
      "Position must be an object of class {.cls r2slides::slide_position}"
    )
  }

  # Process all transformations
  top_fn <- process_transformation(top_transformation, "top_transformation")
  left_fn <- process_transformation(left_transformation, "left_transformation")
  width_fn <- process_transformation(
    width_transformation,
    "width_transformation"
  )
  height_fn <- process_transformation(
    height_transformation,
    "height_transformation"
  )

  # Apply transformations to the position values
  new_top <- top_fn(position@top)
  new_left <- left_fn(position@left)
  new_width <- width_fn(position@width)
  new_height <- height_fn(position@height)

  # Create and return new slide_position object
  slide_position(
    top = new_top,
    left = new_left,
    width = new_width,
    height = new_height
  )
}


#' This function takes transformations for top, left, width, and height, and returns a function that applies those transformations to a slide_position object.
#'
#' @param top_transformation A function to apply to the top value, or a scalar numeric to replace it
#' @param left_transformation A function to apply to the left value, or a scalar numeric to replace it
#' @param width_transformation A function to apply to the width value, or a scalar numeric to replace it
#' @param height_transformation A function to apply to the height value, or a scalar numeric to replace it
#'
#' @returns A function that applies those transformations to a slide_position object.
#'
#' @export
define_relative_transformation_function <- function(
  top_transformation = identity,
  left_transformation = identity,
  width_transformation = identity,
  height_transformation = identity
) {
  function(position) {
    relative_position(
      position,
      top_transformation = top_transformation,
      left_transformation = left_transformation,
      width_transformation = width_transformation,
      height_transformation = height_transformation
    )
  }
}


#' Testing function for how to use define_relative_transformation_function
#'
#' @param position An object of class `r2slides::slide_position`
#'
#' @returns An object of class `r2slides::slide_position`
#'
#' @examples
#' \dontrun{
#' data |>
#'   write_gs("Sheet Name") |>
#'   add_linked_chart(on_slide_url("URL Of Slide"), in_top_left()) |>
#'   add_text("Title", in_top_left() |> chart_annotation_1())
#' }
#' @export
chart_annotation_1 <- define_relative_transformation_function(
  top_transformation = \(x) x - 0.5,
  width_transformation = 0.5,
  height_transformation = 0.25
)


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

#' Is slide_position
#' @description
#' Check if an object is a slide_position object
#'
#' @param x An object to check
#'
#' @returns TRUE if the object is a slide_position, FALSE otherwise
#' @export
is.slide_position <- function(x) {
  inherits(x, "r2slides::slide_position")
}
