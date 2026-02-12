#' @import S7
NULL

#' Slide Position Object
#'
#' A system for working with Google Slides positions and sizes.
#'
#' @param top The position in inches from the top of the slide (top-left corner before rotation)
#' @param left The position in inches from the left of the slide (top-left corner before rotation)
#' @param width The width of the object in inches (intrinsic width, before rotation)
#' @param height The height of the object in inches (intrinsic height, before rotation)
#' @param rotation The rotation angle in degrees (default 0). Positive values rotate clockwise around the object's center.
#' @param convert_slide_size A logical. Optional. Converts the position and size to Google Slides from a PowerPoint (or custom) size if TRUE.
#' @param slide_size_old Old Slide size specifications. Optional. Used for conversion between powerpoint and google slides sizes.
#' @param slide_size New Slide size specifications. Optional.
#'
#' @details
#' When rotation is applied, the object rotates around its center point. The `top`, `left`,
#' `width`, and `height` properties describe the object's intrinsic dimensions before rotation.
#' The rotation is applied as an affine transformation using the scaleX, scaleY, shearX, and
#' shearY computed properties.
#'
#' To get the axis-aligned bounding box that contains the rotated object, use the `bounding_box()` function.
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

    # Rotation in degrees
    rotation = S7::new_property(
      S7::class_double,
      default = 0,
      validator = function(value) {
        if (length(value) != 1) {
          return("rotation must be a single value")
        }
      }
    ),

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

    # Computed properties - represent intrinsic (unrotated) bounds
    top_end = S7::new_property(S7::class_double, getter = function(self) {
      self@top + self@height
    }),
    left_end = S7::new_property(S7::class_double, getter = function(self) {
      self@left + self@width
    }),
    top_emu = S7::new_property(S7::class_double, getter = function(self) {
      self@top * 914400
    }),
    left_emu = S7::new_property(S7::class_double, getter = function(self) {
      self@left * 914400
    }),
    width_emu = S7::new_property(S7::class_double, getter = function(self) {
      self@width * 914400
    }),
    height_emu = S7::new_property(S7::class_double, getter = function(self) {
      self@height * 914400
    }),

    # Affine transform components (computed from rotation)
    scaleX = S7::new_property(S7::class_double, getter = function(self) {
      cos(self@rotation * pi / 180)
    }),
    scaleY = S7::new_property(S7::class_double, getter = function(self) {
      cos(self@rotation * pi / 180)
    }),
    shearX = S7::new_property(S7::class_double, getter = function(self) {
      -sin(self@rotation * pi / 180)
    }),
    shearY = S7::new_property(S7::class_double, getter = function(self) {
      sin(self@rotation * pi / 180)
    }),
    translateX = S7::new_property(S7::class_double, getter = function(self) {
      self@left
    }),
    translateY = S7::new_property(S7::class_double, getter = function(self) {
      self@top
    })
  ),
  constructor = function(
    top,
    left,
    width,
    height,
    rotation = 0,
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
        rotation = rotation,
        slide_height = slide_size[1],
        slide_width = slide_size[2]
      )
    } else {
      new_object(
        S7_object(),
        top = top,
        left = left,
        width = width,
        height = height,
        rotation = rotation
      )
    }
  }
)


#' Calculate Bounding Box for Slide Position Objects
#'
#' Returns the axis-aligned bounding box that contains one or more slide_position objects,
#' accounting for rotation.
#'
#' @param ... One or more slide_position objects
#'
#' @return A slide_position object representing the bounding box (with rotation = 0)
#'
#' @details
#' When objects are rotated, their actual occupied space extends beyond their intrinsic
#' width and height. This function calculates the smallest axis-aligned rectangle that
#' contains all provided objects, including any rotation.
#'
#' All objects must have the same slide_size (slide_width and slide_height).
#'
#' @export
bounding_box <- function(...) {
  objects <- list(...)

  if (length(objects) == 0) {
    cli::cli_abort("At least one slide_position object must be provided")
  }

  # Check all objects are slide_position
  if (!all(sapply(objects, function(x) S7::S7_inherits(x, slide_position)))) {
    cli::cli_abort("All arguments must be slide_position objects")
  }

  # Check all have same slide size
  first_obj <- objects[[1]]
  for (i in seq_along(objects)) {
    obj <- objects[[i]]
    if (
      obj@slide_width != first_obj@slide_width ||
        obj@slide_height != first_obj@slide_height
    ) {
      cli::cli_abort(c(
        x = "All slide_position objects must have the same slide size",
        i = "Object 1: {first_obj@slide_width} x {first_obj@slide_height}",
        i = "Object {i}: {obj@slide_width} x {obj@slide_height}"
      ))
    }
  }

  # Calculate bounding box for each object
  all_corners_x <- c()
  all_corners_y <- c()

  for (obj in objects) {
    if (obj@rotation == 0) {
      # No rotation - just use corners
      all_corners_x <- c(all_corners_x, obj@left, obj@left_end)
      all_corners_y <- c(all_corners_y, obj@top, obj@top_end)
    } else {
      # Calculate rotated corners
      cx <- obj@left + obj@width / 2
      cy <- obj@top + obj@height / 2

      hw <- obj@width / 2
      hh <- obj@height / 2

      angle_rad <- obj@rotation * pi / 180
      cos_a <- cos(angle_rad)
      sin_a <- sin(angle_rad)

      # Four corners (relative to center)
      corners_x <- c(-hw, hw, hw, -hw)
      corners_y <- c(-hh, -hh, hh, hh)

      # Apply rotation and translation
      rotated_x <- corners_x * cos_a - corners_y * sin_a + cx
      rotated_y <- corners_x * sin_a + corners_y * cos_a + cy

      all_corners_x <- c(all_corners_x, rotated_x)
      all_corners_y <- c(all_corners_y, rotated_y)
    }
  }

  # Find overall bounding box
  min_x <- min(all_corners_x)
  max_x <- max(all_corners_x)
  min_y <- min(all_corners_y)
  max_y <- max(all_corners_y)

  slide_position(
    top = min_y,
    left = min_x,
    width = max_x - min_x,
    height = max_y - min_y,
    rotation = 0,
    slide_size = c(first_obj@slide_height, first_obj@slide_width)
  )
}

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

  # Add rotation info if non-zero
  if (x@rotation != 0) {
    cli::cli_text("Rotation: {.val {x@rotation}}")
  }

  cli::cli_text("Left bounds: {.val {x@left}} -> {.val {x@left_end}}")
  cli::cli_text("Top bounds: {.val {x@top}} -> {.val {x@top_end}}")

  if (x@rotation != 0) {
    cli::cli_alert_info(
      "Bounds shown are intrinsic (unrotated). Use {.code bounding_box()} for actual occupied space."
    )
  }

  cat("\n")
  cli::cli_text("Run {.code plot()} to plot the slide position.")

  invisible(x)
}

# Register plot method
S7::method(plot, slide_position) <- function(
  x,
  ...,
  slide_size = NULL,
  colors = NULL
) {
  # Check if ggplot2 is installed
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort(c(
      "x" = "Package {.pkg ggplot2} is required to plot slide positions",
      "i" = "Install it with: {.code install.packages('ggplot2')}"
    ))
  }

  # Determine slide size
  if (is.null(slide_size)) {
    slide_size <- c(x@slide_height, x@slide_width)
  }
  slide_height <- slide_size[1]
  slide_width <- slide_size[2]

  # Combine all positions from ... argument
  additional_positions <- list(...)
  all_positions <- c(list(x), additional_positions)

  # Filter to only slide_position objects
  all_positions <- Filter(
    function(obj) S7::S7_inherits(obj, slide_position),
    all_positions
  )

  if (length(all_positions) == 0) {
    cli::cli_abort("At least one slide_position object must be provided")
  }

  # Default colors
  if (is.null(colors)) {
    colors <- c(
      "#3B82F6",
      "#10B981",
      "#F59E0B",
      "#8B5CF6",
      "#EC4899",
      "#06B6D4",
      "#84CC16",
      "#F97316"
    )
  }

  # Ensure we have enough colors
  if (length(all_positions) > length(colors)) {
    colors <- rep(colors, length.out = length(all_positions))
  }

  # Create data for rectangles
  rect_data <- data.frame()
  edge_data <- data.frame()
  annotation_data <- data.frame()
  info_data <- data.frame()

  for (i in seq_along(all_positions)) {
    pos <- all_positions[[i]]
    color <- colors[i]

    # Store info for display at top
    info_data <- rbind(
      info_data,
      data.frame(
        id = i,
        top = pos@top,
        left = pos@left,
        rotation = pos@rotation,
        color = color
      )
    )

    if (pos@rotation == 0) {
      # Simple rectangle
      rect_data <- rbind(
        rect_data,
        data.frame(
          id = i,
          x = c(pos@left, pos@left_end, pos@left_end, pos@left),
          y = c(pos@top, pos@top, pos@top_end, pos@top_end),
          color = color,
          is_rotated = FALSE
        )
      )

      # Top edge (highlighted)
      edge_data <- rbind(
        edge_data,
        data.frame(
          id = i,
          x = pos@left,
          xend = pos@left_end,
          y = pos@top,
          yend = pos@top,
          color = color
        )
      )

      # Annotation positions (outside the shape)
      # Width annotation - below the bottom edge
      annotation_data <- rbind(
        annotation_data,
        data.frame(
          id = i,
          x = pos@left + pos@width / 2,
          y = pos@top - 0.2,
          label = sprintf("%.2f\"", pos@width),
          angle = 0,
          type = "width",
          color = color
        )
      )

      # Height annotation - to the right of the right edge
      annotation_data <- rbind(
        annotation_data,
        data.frame(
          id = i,
          x = pos@left_end + 0.15,
          y = pos@top + pos@height / 2,
          label = sprintf("%.2f\"", pos@height),
          angle = 90,
          type = "height",
          color = color
        )
      )

      # Rotation annotation (if needed)
      if (pos@rotation != 0) {
        annotation_data <- rbind(
          annotation_data,
          data.frame(
            id = i,
            x = pos@left + pos@width / 2,
            y = pos@top_end + 0.15,
            label = sprintf("%g", pos@rotation),
            angle = 0,
            type = "rotation",
            color = color
          )
        )
      }
    } else {
      # Rotated rectangle - calculate corners
      cx <- pos@left + pos@width / 2
      cy <- pos@top + pos@height / 2
      hw <- pos@width / 2
      hh <- pos@height / 2

      angle_rad <- pos@rotation * pi / 180
      cos_a <- cos(angle_rad)
      sin_a <- sin(angle_rad)

      # Four corners in order: top-left, top-right, bottom-right, bottom-left
      corners_x <- c(-hw, hw, hw, -hw)
      corners_y <- c(-hh, -hh, hh, hh)

      # Apply rotation
      rotated_x <- corners_x * cos_a - corners_y * sin_a + cx
      rotated_y <- corners_x * sin_a + corners_y * cos_a + cy

      rect_data <- rbind(
        rect_data,
        data.frame(
          id = i,
          x = rotated_x,
          y = rotated_y,
          color = color,
          is_rotated = TRUE
        )
      )

      # Top edge (from top-left to top-right, indices 1 to 2)
      edge_data <- rbind(
        edge_data,
        data.frame(
          id = i,
          x = rotated_x[1],
          xend = rotated_x[2],
          y = rotated_y[1],
          yend = rotated_y[2],
          color = color
        )
      )

      # Calculate positions for annotations along rotated axes
      # Width annotation - below bottom edge
      bottom_center_x <- mean(c(rotated_x[3], rotated_x[4]))
      bottom_center_y <- mean(c(rotated_y[3], rotated_y[4]))
      # Move outward perpendicular to bottom edge
      perpendicular_x <- -sin_a * 0.15
      perpendicular_y <- cos_a * 0.15

      annotation_data <- rbind(
        annotation_data,
        data.frame(
          id = i,
          x = bottom_center_x + perpendicular_x,
          y = bottom_center_y + perpendicular_y,
          label = sprintf("%.2f\"", pos@width),
          angle = pos@rotation,
          type = "width",
          color = color
        )
      )

      # Height annotation - to the right of right edge
      right_center_x <- mean(c(rotated_x[2], rotated_x[3]))
      right_center_y <- mean(c(rotated_y[2], rotated_y[3]))
      # Move outward perpendicular to right edge
      perpendicular_x <- cos_a * 0.15
      perpendicular_y <- sin_a * 0.15

      annotation_data <- rbind(
        annotation_data,
        data.frame(
          id = i,
          x = right_center_x + perpendicular_x,
          y = right_center_y + perpendicular_y,
          label = sprintf("%.2f\"", pos@height),
          angle = pos@rotation + 90,
          type = "height",
          color = color
        )
      )

      # Rotation annotation - above top edge
      top_center_x <- mean(c(rotated_x[1], rotated_x[2]))
      top_center_y <- mean(c(rotated_y[1], rotated_y[2]))
      perpendicular_x <- sin_a * 0.2
      perpendicular_y <- -cos_a * 0.2

      annotation_data <- rbind(
        annotation_data,
        data.frame(
          id = i,
          x = top_center_x + perpendicular_x,
          y = top_center_y + perpendicular_y,
          label = sprintf("%g", pos@rotation),
          angle = pos@rotation,
          type = "rotation",
          color = color
        )
      )
    }
  }

  # Create the plot
  p <- ggplot2::ggplot() +
    # Slide background
    ggplot2::annotate(
      "rect",
      xmin = 0,
      xmax = slide_width,
      ymin = 0,
      ymax = slide_height,
      fill = "gray95",
      color = "gray30",
      linewidth = 1
    ) +
    # "SLIDE" text
    ggplot2::annotate(
      "text",
      x = slide_width / 2,
      y = slide_height / 2,
      label = "SLIDE",
      size = 20,
      color = "gray80",
      fontface = "bold",
      angle = atan(slide_height / slide_width) * 180 / pi
    ) +
    ggplot2::scale_y_reverse()

  # Add rectangles
  for (i in seq_along(all_positions)) {
    pos_data <- rect_data[rect_data$id == i, ]
    color <- unique(pos_data$color)

    # Add filled polygon 
    p <- p +
      ggplot2::geom_polygon(
        data = pos_data,
        ggplot2::aes(x = x, y = y),
        fill = paste0(color, "30"), # 30 = ~19% opacity in hex
        color = color,
        linewidth = 0.4
      )
  }

  # Add highlighted top edges (thicker)
  for (i in seq_along(all_positions)) {
    edge <- edge_data[edge_data$id == i, ]
    color <- unique(edge$color)

    p <- p +
      ggplot2::annotate(
        "segment",
        x = edge$x,
        xend = edge$xend,
        y = edge$y,
        yend = edge$yend,
        color = color,
        linewidth = 1.5,
        alpha = 1
      )
  }

  # Add dimension annotations
  p <- p +
    ggplot2::geom_text(
      data = annotation_data,
      ggplot2::aes(x = x, y = y, label = label, angle = angle, color = color),
      size = 3,
      fontface = "bold",
      family = "sans",
      show.legend = FALSE
    ) +
    ggplot2::scale_color_identity()

  # Add info labels at the top
  info_y_start <- -0.7
  for (i in seq_along(all_positions)) {
    info <- info_data[i, ]
    info_text <- sprintf(
      "Object %d: Top: %.2f  Left: %.2f",
      i,
      info$top,
      info$left
    )
    if (info$rotation != 0) {
      info_text <- sprintf("%s  Rotation: %g", info_text, info$rotation)
    }

    p <- p +
      ggplot2::annotate(
        "text",
        x = 0,
        y = info_y_start - (i - 1) * 0.27,
        label = info_text,
        size = 3,
        color = info$color,
        fontface = "bold",
        hjust = 0,
        family = "sans"
      )
  }

  # Add ruler ticks
  # Top ruler
  tick_positions <- seq(0, slide_width, by = 0.5)
  for (tick in tick_positions) {
    tick_height <- if (tick %% 1 == 0) 0.15 else 0.08
    p <- p +
      ggplot2::annotate(
        "segment",
        x = tick,
        xend = tick,
        y = -0.05,
        yend = -0.05 - tick_height,
        color = "gray40",
        linewidth = if (tick %% 1 == 0) 0.5 else 0.3
      )
    if (tick %% 1 == 0) {
      p <- p +
        ggplot2::annotate(
          "text",
          x = tick,
          y = -0.3,
          label = tick,
          size = 2.5,
          color = "gray30"
        )
    }
  }

  # Left ruler
  tick_positions_y <- seq(0, slide_height, by = 0.5)
  for (tick in tick_positions_y) {
    tick_width <- if (tick %% 1 == 0) 0.15 else 0.08
    p <- p +
      ggplot2::annotate(
        "segment",
        x = -0.05,
        xend = -0.05 - tick_width,
        y = tick,
        yend = tick,
        color = "gray40",
        linewidth = if (tick %% 1 == 0) 0.5 else 0.3
      )
    if (tick %% 1 == 0) {
      p <- p +
        ggplot2::annotate(
          "text",
          x = -0.35,
          y = tick,
          label = tick,
          size = 2.5,
          color = "gray30",
          angle = 90
        )
    }
  }

  # Styling - adjust y limits to accommodate info at top
  info_space <- max(1, length(all_positions) * 0.28 + 0.75)

  p <- p +
    ggplot2::coord_fixed(
      xlim = c(-0.6, slide_width + 0.6),
      ylim = c(slide_height + 0.6, -info_space), # Extended for info
      expand = FALSE
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )

  print(p)
  invisible(x)
}






S7::method(convert, list(slide_position, S7::class_list)) <- function(
  from,
  to
) {
  result <- list(
    left = from@left,
    top = from@top,
    width = from@width,
    height = from@height
  )

  # Add rotation if non-zero
  if (from@rotation != 0) {
    result$rotation <- from@rotation
  }

  result
}

S7::method(`+`, list(slide_position, S7::class_numeric)) <- function(e1, e2) {
  slide_position(
    top = e1@top,
    left = e1@left,
    width = e1@width + e2,
    height = e1@height + e2,
    rotation = e1@rotation,
    slide_size = c(e1@slide_height, e1@slide_width)
  )
}



mirror <- S7::new_generic('mirror', 'x', function(x, ...) {
  S7::S7_dispatch()
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
      rotation = -x@rotation, 
      slide_size = c(x@slide_height, x@slide_width)
    )
  } else if (flip_axis == 'Vertical') {
    slide_position(
      top = x@slide_height - x@top - x@height,
      left = x@left,
      width = x@width,
      height = x@height,
      rotation = -x@rotation, 
      slide_size = c(x@slide_height, x@slide_width)
    )
  }
}


#' Creates a slide_position object with defaults in the top left of the slide
#' @param convert_slide_size A logical. Optional.
#' @param slide_size_old Old Slide size specifications. Optional. Used for conversion between powerpoint and google slides sizes
#' @param slide_size New Slide size specifications. Optional.
#'
#' @rdname slide-position-helpers
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


#' @rdname slide-position-helpers
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


#' @rdname slide-position-helpers
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

#' @rdname slide-position-helpers
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


#' @rdname slide-position-helpers
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


#' @rdname slide-position-helpers
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
