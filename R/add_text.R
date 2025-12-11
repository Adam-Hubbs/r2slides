#' Add or update text in a Google Slides presentation
#'
#' @param slide_obj A Google Slides slide object.
#' @param text A character string of text to add.
#' @param position An object of class `r2slides::slide_position`
#' @param element_id Optional. A string ID of an existing text element to update. If element_id is `NULL` a new element will be created.
#' @param text_style Optional. A list of text styling properties.
#' @param verbose Optional. A logical indicating whether to print API responses. Default: TRUE.
#' @param bg_color Optional. A list specifying background color (RGB).
#' @param token Optional. An OAuth2 token. The default uses `r2slides_token()` to find a token.
#' @param call Optional. Call environment used in error messages.
#' @param ... Additional arguments reserved for future expansion.
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
  bg_color = NULL,
  token = NULL,
  call = rlang::caller_env(),
  ...
) {
  # Adds text to the current slide in a presentation
  # If element_id is provided, it will update that text element
  # Otherwise, it creates a new text box at the specified position


  if (!is.character(text)) {
    cli::cli_abort("{.var text} must be a {.code character}", call = call)
  }

  # Validate position object
  if (!inherits(position, "r2slides::slide_position")) {
    cli::cli_abort(
      "Position must be an object of class {.cls r2slides::slide_position}",
      call = call
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
    # Create a new text box
    text_request <- list(
      requests = list(
        list(
          createShape = list(
            objectId = paste0(
              "text_",
              format(Sys.time(), "%Y%m%d%H%M%S"),
              "_",
              sample(1000:9999, 1)
            ),
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
    )

    rsp <- query2(
      endpoint = 'slides.presentations.batchUpdate',
      params = params,
      body = text_request,
      base = 'slides',
      token = token,
      call = call
    )

    # Get the object ID of the created shape
    element_id <- rsp$replies[[1]]$createShape$objectId

    # Format background color (if supplied)
    if (!is.null(bg_color)) {
      background_request <- list(
        updateShapeProperties = list(
          objectId = element_id,
          fields = "shapeBackgroundFill",
          shapeProperties = list(
            shapeBackgroundFill = list(
              solidFill = list(
                color = list(
                  rgbColor = bg_color
                )
              )
            )
          )
        )
      )
    }

    # insert text request
    text_insert_request <-
      list(
        insertText = list(
          objectId = element_id,
          text = text
        )
      )

    #Put requests together
    if (!is.null(bg_color)) {
      text_request <- list(
        requests = list(
          background_request,
          text_insert_request
        )
      )
    } else {
      text_request <- list(
        requests = list(
          text_insert_request
        )
      )
    }

    rsp <- query2(
      endpoint = 'slides.presentations.batchUpdate',
      params = params,
      body = text_request,
      base = 'slides',
      token = token,
      call = call
    )
  } else {
    # Update existing text element
    text_update_request <- list(
      requests = list(
        list(
          deleteText = list(
            objectId = element_id,
            textRange = list(
              type = "ALL"
            )
          )
        ),
        list(
          insertText = list(
            objectId = element_id,
            text = text
          )
        )
      )
    )

    rsp <- query2(
      endpoint = 'slides.presentations.batchUpdate',
      params = params,
      body = text_update_request,
      base = 'slides',
      token = token,
      call = call
    )

    if (verbose == TRUE) {
      print(rsp)
    }
  }

  # Apply text styling if provided
  if (!is.null(text_style)) {
    style_request <- list(
      requests = list(
        list(
          updateTextStyle = list(
            objectId = element_id,
            textRange = list(
              type = "ALL"
            ),
            style = text_style,
            fields = paste(names(text_style), collapse = ",")
          )
        )
      )
    )

    rsp <- query2(
      endpoint = 'slides.presentations.batchUpdate',
      params = params,
      body = style_request,
      base = 'slides',
      token = token,
      call = call
    )
  }

  # Return the element ID for future reference
  return(invisible(slide_obj))
}






#' Apply text styling to all elements on a slide that match the selector function
#' Can be used to apply to every text element containing a +, every element containing a -, only the text parts of all elements, etc. 
#' @param selector A function that takes a text element and returns TRUE if it should be styled.
#' @param style A list of text styling properties.
#' 
#' @export
style_text_if <- function (selector, style) {
  NULL
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
