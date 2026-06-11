#' Add text in a Google Slides presentation
#'
#' Adds text into a new text box on a Google Slides presentation. Text can be formatted by text_style.
#'
#' @param slide_obj A Google Slides slide object.
#' @param text A character string of text to add.
#' @param position An object of class `r2slides::slide_position`
#' @param element_id Optional. A string ID of an existing text element to update. If element_id is `NULL` a new element will be created.
#' @param text_style Optional. A list of text styling properties. One of:
#'     * NULL (the default): Styling is determined by the defaults for the Google Slides presentation
#'     * r2slides::text_style object: List of Text styling
#'     * r2slides::style_rule object: Conditionally formatted object that defined styles and when to use those styles. Resolves to a text_style object.
#' @param order Optional. One of `"front"` or `"back"`. Controls the Z-order of the
#'   created element. Default: `"front"`. Ignored when updating an existing element
#'   via `element_id`.
#' @inheritParams replacement_strategy_params
#' @param ... Additional values available to style_rule objects.
#'
#' @returns The Google Slides slide object (invisibly).
#' @examples
#' if(FALSE) {
#' on_slide_number(2) |>
#'   add_text("Hello there!", position = in_top_left())
#' }
#' @export
add_text <- function(
  slide_obj,
  text,
  position,
  element_id = NULL,
  text_style = NULL,
  order = c("front", "back"),
  replacement_strategy = get_replacement_strategy(),
  match_fn = get_match_fn(),
  ...
) {
  order <- rlang::arg_match(order)

  if (!is.character(text)) {
    cli::cli_abort(
      "{.var text} must be a {.code character}, not a {.cls {class(text)}}"
    )
  }

  if (!inherits(position, "r2slides::slide_position")) {
    cli::cli_abort(
      "Position must be an object of class {.cls r2slides::slide_position} not a {.cls {class(position)}}"
    )
  }

  if (is.null(element_id)) {
    if (
      !apply_replacement(
        slide_obj,
        "TEXT_BOX",
        position,
        strategy = replacement_strategy,
        match_fn = match_fn
      )
    ) {
      return(invisible(slide_obj))
    }
  }

  params <- list(presentationId = slide_obj@presentation$presentation_id)

  result <- build_text_request_items(
    text = text,
    position = position,
    slide_id = slide_obj@slide_id,
    element_id = element_id,
    text_style = text_style,
    ...
  )

  query(
    endpoint = 'slides.presentations.batchUpdate',
    params = params,
    body = list(requests = result$items),
    base = 'slides'
  )

  if (result$new_element && order == 'back') {
    zorder_by_id(
      presentation_id = slide_obj@presentation$presentation_id,
      element_id = result$element_id,
      operation = resolve_zorder_op(order)
    )
  }

  slide_obj@presentation$add_to_ledger(
    element_id = result$element_id,
    slide_id = slide_obj@slide_id,
    element_type = "text",
    element_text = text
  )

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
#' @param order Optional. One of `"front"` or `"back"`. Controls the Z-order of each
#'   created element. Default: `"front"`. Ignored for elements updated via `element_id`.
#' @param pass_strategy Optional. A strategy to pass additional values to style_rule objects.
#' @inheritParams replacement_strategy_params
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
  order = c("front", "back"),
  pass_strategy = c('one', 'all'),
  replacement_strategy = get_replacement_strategy(),
  match_fn = get_match_fn(),
  ...
) {
  order <- rlang::arg_match(order)
  pass_strategy <- rlang::arg_match(pass_strategy)

  # Determine the target length from non-scalar arguments
  obj_lengths <- c(
    text = get_safe_length(text),
    position = get_safe_length(position),
    position_base = get_safe_length(position_base),
    element_id = get_safe_length(element_id),
    text_style = get_safe_length(text_style)
  )

  # Remove NULLs (they have length 0)
  obj_lengths <- obj_lengths[obj_lengths > 0]

  # Check for valid recycling: all obj_lengths should be 1 or the max length
  max_len <- max(obj_lengths)
  invalid <- obj_lengths[obj_lengths != 1 & obj_lengths != max_len]

  if (length(invalid) > 0) {
    cli::cli_abort(
      c(
        "Cannot recycle arguments to a common length.",
        "i" = "All arguments must have length 1 or length {max_len}.",
        "x" = "Invalid lengths: {.val {invalid}}"
      )
    )
  }

  text_recycled <- recycle_arg(text, "text", max_len)
  position_recycled <- recycle_arg(position, "position", max_len)
  position_base_recycled <- recycle_arg(position_base, "position_base", max_len)
  element_id_recycled <- recycle_arg(element_id, "element_id", max_len)
  text_style_recycled <- recycle_arg(text_style, "text_style", max_len)

  # Handle ... based on pass_strategy
  dots <- rlang::list2(...)

  if (pass_strategy == 'one') {
    # Recycle each element of ... individually
    dots_recycled <- purrr::map(dots, recycle_arg, name = "...", max_len = max_len)

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

  keep_mask <- apply_replacement(
    slide_obj,
    "TEXT_BOX",
    final_position,
    element_id_recycled,
    strategy = replacement_strategy,
    match_fn = match_fn
  )
  if (!any(keep_mask)) {
    return(invisible(slide_obj))
  }

  text_recycled <- text_recycled[keep_mask]
  final_position <- final_position[keep_mask]
  element_id_recycled <- element_id_recycled[keep_mask]
  text_style_recycled <- text_style_recycled[keep_mask]
  dots_for_map <- dots_for_map[keep_mask]

  params <- list(presentationId = slide_obj@presentation$presentation_id)

  build_results <- tryCatch(
    purrr::pmap(
      list(
        text = text_recycled,
        final_position = final_position,
        element_id = element_id_recycled,
        text_style = text_style_recycled,
        dots = dots_for_map
      ),
      function(text, final_position, element_id, text_style, dots) {
        rlang::exec(
          build_text_request_items,
          text = text,
          position = final_position,
          slide_id = slide_obj@slide_id,
          element_id = element_id,
          text_style = text_style,
          !!!dots
        )
      }
    ),
    error = function(error) {
      cli::cli_abort(c("x" = "Failed to add text to slide"), parent = error)
    }
  )

  all_items <- purrr::list_c(purrr::map(build_results, "items"))

  query(
    endpoint = 'slides.presentations.batchUpdate',
    params = params,
    body = list(requests = all_items),
    base = 'slides'
  )

  purrr::walk(build_results, \(result) {
    if (result$new_element && order == 'back') {
      zorder_by_id(
        presentation_id = slide_obj@presentation$presentation_id,
        element_id = result$element_id,
        operation = resolve_zorder_op(order)
      )
    }
  })

  purrr::walk(
    purrr::map2(
      build_results,
      text_recycled,
      \(result, txt) list(result = result, text = txt)
    ),
    \(x) {
      slide_obj@presentation$add_to_ledger(
        element_id = x$result$element_id,
        slide_id = slide_obj@slide_id,
        element_type = "text",
        element_text = x$text
      )
    }
  )

  return(invisible(slide_obj))
}


# Builds the list of batchUpdate request items for a single text element without
# making any API calls. Returns a list with: element_id, new_element flag, and
# items (the list of request objects to include in a batchUpdate requests array).
build_text_request_items <- function(
  text,
  position,
  slide_id,
  element_id = NULL,
  text_style = NULL,
  ...
) {
  new_element <- is.null(element_id)

  if (new_element) {
    element_id <- paste0(
      "text_",
      format(Sys.time(), "%Y%m%d%H%M%S"),
      "_",
      sample(1000:9999, 1)
    )
    shape_items <- list(
      list(
        createShape = list(
          objectId = element_id,
          shapeType = "TEXT_BOX",
          elementProperties = list(
            pageObjectId = slide_id,
            size = list(
              width = list(magnitude = position@width_emu, unit = "EMU"),
              height = list(magnitude = position@height_emu, unit = "EMU")
            ),
            transform = list(
              scaleX = position@scaleX,
              scaleY = position@scaleY,
              shearX = position@shearX,
              shearY = position@shearY,
              translateX = position@left_emu,
              translateY = position@top_emu,
              unit = "EMU"
            )
          )
        )
      )
    )
  } else {
    shape_items <- list()
  }

  insert_items <- list(
    list(
      insertText = list(
        objectId = element_id,
        text = text
      )
    )
  )

  if (!is.null(text_style)) {
    if (inherits(text_style, "r2slides::text_style")) {
      text_style <- style_rule(default = text_style)
    }
    raw_style <- create_styling_request(
      style_rule = text_style,
      text = text,
      element_id = element_id,
      ...
    )
    style_items <- purrr::map(seq_along(raw_style), \(i) raw_style[i])
  } else {
    style_items <- list()
  }

  list(
    element_id = element_id,
    new_element = new_element,
    items = c(shape_items, insert_items, style_items)
  )
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
            "x" = "A relative slide position function call was passed to the position argument. Please pass the name of the function, or a {.cls r2slides::slide_position} object instead.",
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


# Recycle each argument to max_len
recycle_arg <- function(arg, name, max_len) {
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
    } else if (
      inherits(arg, "r2slides::slide_position") ||
        inherits(arg, "r2slides::text_style") ||
        inherits(arg, "r2slides::style_rule") ||
        is.function(arg) ||
        rlang::is_quosure(arg)
    ) {
      return(list(arg))
    } else {
      return(as.list(arg))
    }
  }
  list(arg)
}
