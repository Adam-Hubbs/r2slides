#' Add or update text in a Google Slides presentation
#'
#' @param text A character string of text to add.
#' @param presentation Optional. A Google Slides presentation object. By default it searches for
#' an environment named `google_presentation`.
#' @param element_id Optional. A string ID of an existing text element to update. If element_id is `NULL` a new element will be created.
#' @param left Optional. A numeric position from left edge in inches. Default: 1.
#' @param top Optional. A numeric position from top edge in inches. Default: 1.
#' @param width Optional. A numeric width in inches. Default: 2.
#' @param height Optional. A numeric height in inches. Default: 0.5.
#' @param text_style Optional. A list of text styling properties.
#' @param verbose Optional. A logical indicating whether to print API responses. Default: TRUE.
#' @param report_style One of `"qualtrics"`, `"municipal"`, or `"y2"`.
#' @param bg_color Optional. A list specifying background color (RGB).
#' @param convert_slide_size Optional. A logical indicating whether to convert dimensions from PowerPoint into Google Slides. If `TRUE`, then positioning would work identically to a PowerPoint presentation. If `FALSE` it uses the raw inches directly. Default: TRUE.
#' @param slide_size Optional. A list containing slide size specifications for converting. In the form of list(x_height = 7.5, x_width = 13.3, y_height = 5, y_width = 9). `x_` indicates converting from and `y_` indicates converting too. You can override this to convert between custom slide sizes. If left blank it converts from PowerPoint to Google Slides.
#' @param ... Additional arguments reserved for future expansion.
#'
#' @returns
#' The element ID invisibly. Will error if no slide is currently selected or if
#' text is not a character string.
#'
#' @export
add_text <- function(
    text,
    presentation = google_presentation,
    element_id = NULL,
    left = 1,
    top = 1,
    width = 2,
    height = 0.5,
    text_style = NULL,
    verbose = TRUE,
    report_style = c('qualtrics', 'municipal', 'y2'),
    bg_color = NULL,
    convert_slide_size = TRUE,
    slide_size = NULL,
    ...
) {
  # Adds text to the current slide in a presentation
  # If element_id is provided, it will update that text element
  # Otherwise, it creates a new text box at the specified position

  ## Background Color currently only works if you are making new text, not if you supply an id to update existing text
  report_style <- rlang::arg_match(report_style)

  validatePresentation(deparse(substitute(presentation)))

  if (
    is.null(presentation$current_slide_id) ||
    is.na(presentation$current_slide_id)
  ) {
    cli::cli_abort(
      "No current slide selected. Please create or select a slide first."
    )
  }

  if (!is.character(text)) {
    cli::cli_abort("{.var text} must be a {.code character}")
  }

  # Validate position and size
  if (!all(vapply(list(left, top, width, height), is.numeric, logical(1)))) {
    cli::cli_abort(
      "Position and size arguments must contain only numeric values"
    )
  }

  # Convert from PowerPoint slide size to Google Slide size
  if (convert_slide_size == TRUE) {
    left <- correct_slide_size(left, dim = 'width', slide_size)
    top <- correct_slide_size(top, dim = 'height', slide_size)
    width <- correct_slide_size(width, dim = 'width', slide_size)
    height <- correct_slide_size(height, dim = 'height', slide_size)
  }

  # Convert from inches to PT's which the API needs
  left <- in_to_pt(left)
  top <- in_to_pt(top)
  width <- in_to_pt(width)
  height <- in_to_pt(height)

  # Get current slide_id
  slide_id <- presentation$current_slide_id

  params <- list(presentationId = presentation$presentation_id)

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
      #call = #Leave null so caller_env points to this function as the lowest exported function?
    )


    # Get the object ID of the created shape
    # TODO CHECK THAT THIS WORKS WITH QUERY2!!!
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
      #call = #Leave null so caller_env points to this function as the lowest exported function?
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
      #call = #Leave null so caller_env points to this function as the lowest exported function?
    )

    if (verbose == TRUE) {
      print(rsp)
    }
  }

  # Apply text styling if provided
  #Do we want this function to have ownership of this? i.e. creating the text_style object (or doing it directly in the request?)
  #It would avoid duplication across the more abstracted text functions
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
      #call = #Leave null so caller_env points to this function as the lowest exported function?
    )

  }

  # Return the element ID for future reference
  return(invisible(element_id))
}


#' Add a title to a Google Slide
#'
#' @param text A single string for the title text. Optional, defaults to "Title".
#' @param presentation Optional. A Google Slides presentation object. By default it searches for
#' an environment named `google_presentation`.
#' @param title_font_size Optional. Font size in points.
#' @param title_font_family Optional. Font family name.
#' @param title_font_bold Optional. Whether text should be bold.
#' @param title_color Optional. Title text color as hex code.
#' @param title_bg_color Optional. Title background color as hex code.
#' @param title_left Optional. Left position of title.
#' @param title_top Optional. Top position of title.
#' @param title_width Optional. Width of title element.
#' @param title_height Optional. Height of title element.
#' @param report_style One of `"qualtrics"`, `"municipal"`, or `"y2"`.
#' @param verbose Optional. A logical indicating whether to print API responses. Default: `TRUE`.
#' @param convert_slide_size Optional. A logical indicating whether to convert dimensions from PowerPoint into Google Slides. If `TRUE`, then positioning would work identically to a PowerPoint presentation. If `FALSE` it uses the raw inches directly. Default: TRUE.
#' @param slide_size Optional. A list containing slide size specifications for converting. In the form of list(x_height = 7.5, x_width = 13.3, y_height = 5, y_width = 9). `x_` indicates converting from and `y_` indicates converting too. You can override this to convert between custom slide sizes. If left blank it converts from PowerPoint to Google Slides.
#' @param ... Additional arguments reserved for future expansion.
#'
#' @returns
#' The element_id of the created title (invisibly)
#'
#' @export
add_title <- function(
    text = "Title",
    presentation = google_presentation,
    title_font_size = NULL,
    title_font_family = NULL,
    title_font_bold = NULL,
    title_color = NULL,
    title_bg_color = NULL,
    title_left = NULL,
    title_top = NULL,
    title_width = NULL,
    title_height = NULL,
    report_style = c('qualtrics', 'municipal', 'y2'),
    verbose = TRUE,
    convert_slide_size = TRUE,
    slide_size = NULL,
    ...
) {
  report_style <- rlang::arg_match(report_style)

  #Set defaults by theme
  args <- list(
    title_font_size = title_font_size,
    title_font_family = title_font_family,
    title_font_bold = title_font_bold,
    title_color = title_color,
    title_bg_color = title_bg_color,
    title_left = title_left,
    title_top = title_top,
    title_width = title_width,
    title_height = title_height
  )

  list2env(
    set_defaults(args, type = 'title', report_style = report_style),
    envir = environment()
  )

  #font_bold does not work yet

  #Translate hex colors to rbg
  color <- hex_to_rgb(title_color)
  bg_color <- hex_to_rgb(title_bg_color)

  text_style = list(
    fontFamily = title_font_family,
    fontSize = list(magnitude = title_font_size, unit = "PT"),
    foregroundColor = list(
      opaqueColor = list(
        rgbColor = color
      )
    )
  )

  add_text(
    text = text,
    left = title_left,
    top = title_top,
    width = title_width,
    height = title_height,
    text_style = text_style,
    bg_color = bg_color,
    report_style = report_style,
    verbose = verbose,
    convert_slide_size = convert_slide_size,
    slide_size = slide_size
  )
}


#' Add commentary text to a Google Slide
#'
#' @param text A single string. The text to add as commentary.
#' @param presentation A Google Slides presentation object.
#' @param commentary_font_size A numeric value for font size in points. Optional.
#' @param commentary_font_family A single string specifying font family. Optional.
#' @param commentary_color A single string with hex color code for text. Optional.
#' @param commentary_bg_color A single string with hex color code for background. Optional.
#' @param commentary_left A numeric value for left position. Optional.
#' @param commentary_top A numeric value for top position. Optional.
#' @param commentary_width A numeric value for width. Optional.
#' @param commentary_height A numeric value for height. Optional.
#' @param report_style One of `"qualtrics"`, `"municipal"`, or `"y2"`.
#' @param verbose Optional. A logical indicating whether to print API responses. Default: TRUE.
#' @param convert_slide_size Optional. A logical indicating whether to convert dimensions from PowerPoint into Google Slides. If `TRUE`, then positioning would work identically to a PowerPoint presentation. If `FALSE` it uses the raw inches directly. Default: TRUE.
#' @param slide_size Optional. A list containing slide size specifications for converting. In the form of list(x_height = 7.5, x_width = 13.3, y_height = 5, y_width = 9). `x_` indicates converting from and `y_` indicates converting too. You can override this to convert between custom slide sizes. If left blank it converts from PowerPoint to Google Slides.
#' @param ... Additional arguments reserved for future expansion.
#'
#' @returns The element_id of the commentary text (invisibly)
#'
#' @export
add_commentary <- function(
    text = "Commentary",
    presentation = google_presentation,
    commentary_font_size = 14,
    commentary_font_family = NULL,
    commentary_color = NULL,
    commentary_bg_color = NULL,
    commentary_left = NULL,
    commentary_top = NULL,
    commentary_width = NULL,
    commentary_height = NULL,
    report_style = c('qualtrics', 'municipal', 'y2'),
    verbose = TRUE,
    convert_slide_size = TRUE,
    slide_size = NULL,
    ...
) {
  report_style <- rlang::arg_match(report_style)

  #Set defaults by theme
  args <- list(
    commentary_font_family = commentary_font_family,
    commentary_color = commentary_color,
    commentary_bg_color = commentary_bg_color,
    commentary_left = commentary_left,
    commentary_top = commentary_top,
    commentary_width = commentary_width,
    commentary_height = commentary_height
  )

  list2env(
    set_defaults(args, type = 'commentary', report_style = report_style),
    envir = environment()
  )

  #Translate hex colors to rbg
  color <- hex_to_rgb(commentary_color)
  bg_color <- hex_to_rgb(commentary_bg_color)

  text_style = list(
    fontFamily = commentary_font_family,
    fontSize = list(magnitude = commentary_font_size, unit = "PT"),
    foregroundColor = list(
      opaqueColor = list(
        rgbColor = color
      )
    )
  )

  add_text(
    text = text,
    left = commentary_left,
    top = commentary_top,
    width = commentary_width,
    height = commentary_height,
    text_style = text_style,
    bg_color = bg_color,
    report_style = report_style,
    verbose = verbose,
    convert_slide_size = convert_slide_size,
    slide_size = slide_size
  )
}


#' Add a footer to a Google Slide
#'
#' @param text A string. Optional, defaults to "Footer".
#' @param presentation A Google Slides presentation object.
#' @param footer_font_size A numeric value for font size in points. Optional, defaults to 10.
#' @param footer_font_family A string specifying the font family. Optional.
#' @param footer_color A string specifying the color (hex code). Optional.
#' @param footer_left A numeric value for left position. Optional.
#' @param footer_top A numeric value for top position. Optional.
#' @param footer_width A numeric value for width. Optional.
#' @param footer_height A numeric value for height. Optional.
#' @param report_style One of `"qualtrics"`, `"municipal"`, or `"y2"`.
#' @param bg_color A string specifying background color. Optional.
#' @param verbose Optional. A logical indicating whether to print API responses. Default: `TRUE`.
#' @param convert_slide_size Optional. A logical indicating whether to convert dimensions from PowerPoint into Google Slides. If `TRUE`, then positioning would work identically to a PowerPoint presentation. If `FALSE` it uses the raw inches directly. Default: TRUE.
#' @param slide_size Optional. A list containing slide size specifications for converting. In the form of list(x_height = 7.5, x_width = 13.3, y_height = 5, y_width = 9). `x_` indicates converting from and `y_` indicates converting too. You can override this to convert between custom slide sizes. If left blank it converts from PowerPoint to Google Slides.
#' @param ... Additional arguments reserved for future expansion.
#'
#' @returns
#' The element_id of the created footer (invisibly)
#'
#' @export
add_footer <- function(
    text = "Footer",
    presentation = google_presentation,
    footer_font_size = 10,
    footer_font_family = NULL,
    footer_color = NULL,
    footer_left = NULL,
    footer_top = NULL,
    footer_width = NULL,
    footer_height = NULL,
    report_style = c('qualtrics', 'municipal', 'y2'),
    bg_color = NULL,
    verbose = TRUE,
    convert_slide_size = TRUE,
    slide_size = NULL,
    ...
) {
  report_style <- rlang::arg_match(report_style)

  #Set defaults by theme
  args <- list(
    footer_font_size = footer_font_size,
    footer_font_family = footer_font_family,
    footer_color = footer_color,
    footer_left = footer_left,
    footer_top = footer_top,
    footer_width = footer_width,
    footer_height = footer_height
  )

  list2env(
    set_defaults(args, type = 'footer', report_style = report_style),
    envir = environment()
  )

  #Translate hex colors to rbg
  color <- hex_to_rgb(footer_color)

  text_style = list(
    fontFamily = footer_font_family,
    fontSize = list(magnitude = footer_font_size, unit = "PT"),
    foregroundColor = list(
      opaqueColor = list(
        rgbColor = color
      )
    )
  )

  add_text(
    text = text,
    left = footer_left,
    top = footer_top,
    width = footer_width,
    height = footer_height,
    text_style = text_style,
    bg_color = bg_color,
    report_style = report_style,
    verbose = verbose,
    convert_slide_size = convert_slide_size,
    slide_size = slide_size
  )
}

