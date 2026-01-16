#' Add or update text in a Google Slides presentation
#'
#' @param slide_obj A Google Slides slide object.
#' @param text A character string of text to add.
#' @param position An object of class `r2slides::slide_position`
#' @param element_id Optional. A string ID of an existing text element to update. If element_id is `NULL` a new element will be created.
#' @param text_style Optional. A list of text styling properties.
#' @param verbose Optional. A logical indicating whether to print API responses. Default: TRUE.
#' @param token Optional. An OAuth2 token. The default uses `r2slides_token()` to find a token.
#' @param debug Optional. A logical indicating whether to print debug messages. Default: FALSE.
#' @param ... Additional values available to style_rule objects.
#'
#' @returns The Google Slides slide object (invisibly).
#'
#' @export
add_text <- function(
  slide_obj,
  text,
  position,
  element_id = NULL,
  text_style = NULL,
  verbose = TRUE,
  token = NULL,
  debug = FALSE,
  ...
) {
  # Adds text to the current slide in a presentation
  # If element_id is provided, it will update that text element
  # Otherwise, it creates a new text box at the specified position

  if (!is.character(text)) {
    cli::cli_abort(
      "{.var text} must be a {.code character}, not a {.cls {class(text)}}"
    )
  }

  # Validate position object
  if (!inherits(position, "r2slides::slide_position")) {
    cli::cli_abort(
      "Position must be an object of class {.cls r2slides::slide_position} not a {.cls {class(position)}}"
    )
  }

  # Extract position values
  left <- position@left_pt
  top <- position@top_pt
  width <- position@width_pt
  height <- position@height_pt

  # Get current slide_id
  slide_id <- slide_obj$slide_id

  params <- list(presentationId = slide_obj$presentation_id)

  if (is.null(element_id)) {
    element_id <- paste0(
      "text_",
      format(Sys.time(), "%Y%m%d%H%M%S"),
      "_",
      sample(1000:9999, 1)
    )
    shape_request <-
      list(
        list(
          createShape = list(
            objectId = element_id,
            shapeType = "TEXT_BOX",
            elementProperties = list(
              pageObjectId = slide_id,
              size = list(
                width = list(magnitude = width, unit = "PT"),
                height = list(magnitude = height, unit = "PT")
              ),
              transform = list(
                scaleX = 1,
                scaleY = 1,
                translateX = left,
                translateY = top,
                unit = "PT"
              )
            )
          )
        )
      )
  }

  insert_text_request <-
    list(
      list(
        insertText = list(
          objectId = element_id,
          text = text
        )
      )
    )

  if (!is.null(text_style)) {
    # if it is a text_style object, convert it to a style_rule object
    if (inherits(text_style, "r2slides::text_style")) {
      text_style <- style_rule(default = text_style)
    }
    text_style_request <-
      create_styling_request(
        style_rule = text_style,
        text = text,
        element_id = element_id,
        ...
      )
  }

  shape_request <- list(requests = purrr::compact(shape_request))
  insert_text_request <- list(requests = purrr::compact(insert_text_request))

  text_style_request <- purrr::compact(text_style_request)

  all_text_style_requests <- NULL

  for (i in 1:length(text_style_request)) {
    all_text_style_requests <- c(
      all_text_style_requests,
      list(list(requests = text_style_request[i]))
    )
  }

  query(
    endpoint = 'slides.presentations.batchUpdate',
    params = params,
    body = shape_request,
    base = 'slides',
    token = token
  )

  query(
    endpoint = 'slides.presentations.batchUpdate',
    params = params,
    body = insert_text_request,
    base = 'slides',
    token = token
  )

  purrr::walk(all_text_style_requests, \(x) {
    query(
      endpoint = 'slides.presentations.batchUpdate',
      params = params,
      body = x,
      base = 'slides',
      token = token
    )
  })

  return(invisible(slide_obj))
}


#' Add or update text in a Google Slides presentation
#'
#' @param slide_obj A Google Slides slide object.
#' @param text A vector of character strings to add.
#' @param position A vector of objects of class `r2slides::slide_position`
#' @param position_base A vector of objects of class `r2slides::slide_position`
#' @param element_id Optional. A vector of string IDs of an existing text element to update. If element_id is `NULL` a new element will be created.
#' @param text_style Optional. A vector of text_style or style_rule objects.
#' @param verbose Optional. A logical indicating whether to print API responses. Default: TRUE.
#' @param token Optional. An OAuth2 token. The default uses `r2slides_token()` to find a token.
#' @param pass_strategy Optional. A strategy to pass additional values to style_rule objects.
#' @param debug Optional. A logical indicating whether to print debug messages. Default: FALSE.
#' @param ... Additional values available to style_rule objects.
#'
#' @returns The Google Slides slide object (invisibly).
#'
#' @export
add_text_multi <- function(
  slide_obj,
  text,
  position,
  position_base = NULL,
  element_id = NULL,
  text_style = NULL,
  verbose = TRUE,
  token = NULL,
  pass_strategy = c('one', 'all'),
  debug = FALSE,
  ...
) {
  pass_strategy <- rlang::arg_match(pass_strategy)

  # Determine the target length from non-scalar arguments
  lengths <- c(
    text = get_safe_length(text),
    position = get_safe_length(position),
    position_base = get_safe_length(position_base),
    element_id = get_safe_length(element_id),
    text_style = get_safe_length(text_style)
  )

  # Remove NULLs (they have length 0)
  lengths <- lengths[lengths > 0]

  # Check for valid recycling: all lengths should be 1 or the max length
  max_len <- max(lengths)
  invalid <- lengths[lengths != 1 & lengths != max_len]

  if (length(invalid) > 0) {
    cli::cli_abort(
      c(
        "Cannot recycle arguments to a common length.",
        "i" = "All arguments must have length 1 or length {max_len}.",
        "x" = "Invalid lengths: {.val {invalid}}"
      )
    )
  }

  # Recycle each argument to max_len
  recycle_arg <- function(arg, name) {
    if (is.null(arg)) {
      return(rep(list(NULL), max_len))
    }
    len <- length(arg)
    if (len == 1 && max_len > 1) {
      return(rep(list(arg), max_len))
    } else if (len == max_len) {
      # Convert to list if not already
      if (is.list(arg)) {
        return(arg)
      } else if (inherits(arg, "r2slides::slide_position") | inherits(arg, "r2slides::text_style") | inherits(arg, "r2slides::style_rule") | is.function(arg) | rlang::is_quosure(arg)) {
        return(list(arg))
      } else {
        return(as.list(arg))
      }
      return(as.list(arg))
    }
    return(list(arg))
  }

  text_recycled <- recycle_arg(text, "text")
  position_recycled <- recycle_arg(position, "position")
  position_base_recycled <- recycle_arg(position_base, "position_base")
  element_id_recycled <- recycle_arg(element_id, "element_id")
  text_style_recycled <- recycle_arg(text_style, "text_style")

  # Handle ... based on pass_strategy
  dots <- rlang::list2(...)

  if (pass_strategy == 'one') {
    # Recycle each element of ... individually
    dots_recycled <- purrr::map(dots, recycle_arg, name = "...")

    # Transpose so each iteration gets one element from each dots argument
    if (length(dots) > 0) {
      dots_for_map <- purrr::transpose(dots_recycled)
    } else {
      dots_for_map <- vector("list", max_len)
    }
  } else {
    # pass_strategy == 'all': pass entire ... to each iteration
    dots_for_map <- rep(list(dots), max_len)
  }

  final_position <- purrr::pmap(
    list(position_recycled, position_base_recycled),
    function(position, position_base) {
      if (rlang::is_function(position)) {
        if (rlang::is_function(position_base)) {
          final_position <- position_base() |> position()
        } else {
          final_position <- position(position_base)
        }
      } else {
        position
      }
    }
  )

  # Now map over recycled arguments
  results <- tryCatch(
    purrr::pmap(
      list(
        text = text_recycled,
        final_position = final_position,
        element_id = element_id_recycled,
        text_style = text_style_recycled,
        dots = dots_for_map
      ),
      function(text, final_position, element_id, text_style, dots) {
        add_text(
          slide_obj = slide_obj,
          text = text,
          position = final_position,
          element_id = element_id,
          text_style = text_style,
          verbose = verbose,
          token = token,
          debug = debug,
          ... = !!!dots
        )
      }
    ),
    error = function(error) {
      cli::cli_abort(c("x" = "Failed to add text to slide"), parent = error)
    }
  )

  return(invisible(results[[1]]))
}



#' Get the length of an argument
#'
#' @param arg An argument to get the length of.
#'
#' @export
get_safe_length <- function(arg) {
  tryCatch(
    {
      if (is.null(arg)) {
        return(0L)
      }
      if (rlang::is_function(arg)) {
        return(1L)
      }
      if (rlang::is_quosure(arg)) {
        return(1L)
      }
      length(arg)
    },
    error = function(e) {
      if (grepl("\"position\" is missing", e$message)) {
        func_name <- "in_top_left"

        if (!is.null(e$call)) {
          func_name <- deparse1(e$call[[1]])
        }

        cli::cli_abort(
          c(
            "x" = "A realtive slide position function call was passed to the position argument. Please pass the name of the function, or a {.cls r2slides::slide_position} object instead.",
            "i" = "  - {.strong Good}: position = {func_name}",
            "i" = "  - {.strong Bad}: position = {func_name}()"
          ),
          parent = e,
          call = rlang::caller_env(5) # First 3 are tryCatch internals. 4 is get_safe_length(). We want 5.
        )
      } else {
        cli::cli_abort(
          c(
            "x" = "Error evaluating length of {.var {deparse(substitute(arg))}}"
          ),
          call = rlang::caller_env(5)
        )
      }
    }
  )
}



#' Add a title to a Google Slide
#'
#' @param text A single string for the title text. Optional, defaults to "Title".
#' @param presentation Optional. A Google Slides presentation object. By default it searches for
#' an environment named `google_presentation`.
#' @param position An object of class `r2slides::slide_position`. If `NULL`, default position will be created based on `report_style`.
#' @param title_font_size Optional. Font size in points.
#' @param title_font_family Optional. Font family name.
#' @param title_font_bold Optional. Whether text should be bold.
#' @param title_color Optional. Title text color as hex code.
#' @param title_bg_color Optional. Title background color as hex code.
#' @param report_style One of `"qualtrics"`, `"municipal"`, or `"y2"`.
#' @param verbose Optional. A logical indicating whether to print API responses. Default: `TRUE`.
#' @param token Optional. An OAuth2 token. The default uses `r2slides_token()` to find a token.
#' @param call Optional. Call environment used in error messages.
#' @param ... Additional arguments reserved for future expansion.
#'
#' @returns
#' The element_id of the created title (invisibly)
#'
#' @export
add_title <- function(
  text = "Title",
  presentation = google_presentation,
  position = NULL,
  title_font_size = NULL,
  title_font_family = NULL,
  title_font_bold = NULL,
  title_color = NULL,
  title_bg_color = NULL,
  report_style = c('qualtrics', 'municipal', 'y2'),
  verbose = TRUE,
  token = NULL,
  call = rlang::caller_env(),
  ...
) {
  cli::cli_abort(
    "This function is deprecated. Please use `add_text()` instead.",
    call = call
  )
}


#' Add commentary text to a Google Slide
#'
#' @param text A single string. The text to add as commentary.
#' @param presentation A Google Slides presentation object.
#' @param position An object of class `r2slides::slide_position`. If `NULL`, default position will be created based on `report_style`.
#' @param commentary_font_size A numeric value for font size in points. Optional.
#' @param commentary_font_family A single string specifying font family. Optional.
#' @param commentary_color A single string with hex color code for text. Optional.
#' @param commentary_bg_color A single string with hex color code for background. Optional.
#' @param report_style One of `"qualtrics"`, `"municipal"`, or `"y2"`.
#' @param verbose Optional. A logical indicating whether to print API responses. Default: TRUE.
#' @param token Optional. An OAuth2 token. The default uses `r2slides_token()` to find a token.
#' @param call Optional. Call environment used in error messages.
#' @param ... Additional arguments reserved for future expansion.
#'
#' @returns The element_id of the commentary text (invisibly)
#'
#' @export
add_commentary <- function(
  text = "Commentary",
  presentation = google_presentation,
  position = NULL,
  commentary_font_size = 14,
  commentary_font_family = NULL,
  commentary_color = NULL,
  commentary_bg_color = NULL,
  report_style = c('qualtrics', 'municipal', 'y2'),
  verbose = TRUE,
  token = NULL,
  call = rlang::caller_env(),
  ...
) {
  cli::cli_abort(
    "This function is deprecated. Please use `add_text()` instead.",
    call = call
  )
}


#' Add a footer to a Google Slide
#'
#' @param text A string. Optional, defaults to "Footer".
#' @param presentation A Google Slides presentation object.
#' @param position An object of class `r2slides::slide_position`. If `NULL`, default position will be created based on `report_style`.
#' @param footer_font_size A numeric value for font size in points. Optional, defaults to 10.
#' @param footer_font_family A string specifying the font family. Optional.
#' @param footer_color A string specifying the color (hex code). Optional.
#' @param report_style One of `"qualtrics"`, `"municipal"`, or `"y2"`.
#' @param bg_color A string specifying background color. Optional.
#' @param verbose Optional. A logical indicating whether to print API responses. Default: `TRUE`.
#' @param token Optional. An OAuth2 token. The default uses `r2slides_token()` to find a token.
#' @param call Optional. Call environment used in error messages.
#' @param ... Additional arguments reserved for future expansion.
#'
#' @returns
#' The element_id of the created footer (invisibly)
#'
#' @export
add_footer <- function(
  text = "Footer",
  presentation = google_presentation,
  position = NULL,
  footer_font_size = 10,
  footer_font_family = NULL,
  footer_color = NULL,
  report_style = c('qualtrics', 'municipal', 'y2'),
  bg_color = NULL,
  verbose = TRUE,
  token = NULL,
  call = rlang::caller_env(),
  ...
) {
  cli::cli_abort(
    "This function is deprecated. Please use `add_text()` instead.",
    call = call
  )
}
