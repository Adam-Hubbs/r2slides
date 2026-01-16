
#' Create a new slide in a Google Slides presentation
#'
#' @param presentation Optional. A Google Slides presentation object. By default it searches for
#' an environment named `google_presentation`.
#' @param layout Optional. A character string specifying the slide layout. Default is "TITLE_AND_BODY".
#' @param verbose Optional. A logical value indicating whether to print status updates.
#' @param master Currently unused; must be NULL. Reserved for officer compatibility. (May drop in future)
#' @param ... Additional arguments reserved for future expansion, currently unused.
#'
#' @returns
#' Updates the presentation object with the new slide ID, invisibly.
#' Will error if layout is not a character string or if master is not NULL.
#'
#' @export
new_slide <- function(
    presentation = google_presentation,
    layout = "BLANK",
    verbose = TRUE,
    master = NULL,
    ...
) {
  validatePresentation(deparse(substitute(presentation)))

  if (!is.null(master)) {
    cli::cli_abort("{.var master} is reserved for officer compatibility")
  }

  # if (length(list(...)) > 0) {
  #   cli::cli_abort("{.var ...} is reserved for future expansion")
  # }

  if (!is.character(layout)) {
    cli::cli_abort("{.var layout} must be a {.code character}")
  }

  add_slide_request <- list(
    requests = list(
      list(
        createSlide = list(
          slideLayoutReference = list(
            predefinedLayout = layout
          )
        )
      )
    )
  )

  params <- list(presentationId = presentation$presentation_id)

  rsp <- query(
    endpoint = 'slides.presentations.batchUpdate',
    params = params,
    body = add_slide_request,
    base = 'slides',
    #call = #Leave null so caller_env points to this function as the lowest exported function?
  )

  #Update slides list
  #Update current slide
  presentation$current_slide_id <- rsp$replies[[1]]$createSlide$objectId
  presentation$slide_ids <- c(
    presentation$slide_ids,
    presentation$current_slide_id
  )
}


#' Create a slide with title, commentary, and footer
#'
#' @param presentation Optional. A Google Slides presentation object. By default it searches for
#' an environment named `google_presentation`.
#' @param layout One of the available Google Slides layouts. Optional.
#' @param title A single string for the slide title. Optional.
#' @param commentary A single string for the slide commentary. Optional.
#' @param footer A single string for the slide footer. Optional.
#' @param report_style One of `"qualtrics"`, `"municipal"`, or `"y2"`. Defaults to `qualtrics`.
#' @param verbose A logical indicating whether to print progress messages. Optional.
#' @param convert_slide_size A logical. Optional.
#' @param slide_size Slide size specifications. Optional.
#' @param title_font_size Title font size. Optional.
#' @param title_font_family Title font family. Optional.
#' @param title_font_bold Title font weight. Optional.
#' @param title_color Title text color. Optional.
#' @param title_bg_color Title background color. Optional.
#' @param title_left Title left position. Optional.
#' @param title_top Title top position. Optional.
#' @param title_width Title width. Optional.
#' @param title_height Title height. Optional.
#' @param commentary_font_size Commentary font size. Optional, defaults to 14.
#' @param commentary_font_family Commentary font family. Optional.
#' @param commentary_color Commentary text color. Optional.
#' @param commentary_bg_color Commentary background color. Optional.
#' @param commentary_left Commentary left position. Optional.
#' @param commentary_top Commentary top position. Optional.
#' @param commentary_width Commentary width. Optional.
#' @param commentary_height Commentary height. Optional.
#' @param footer_font_size Footer font size. Optional, defaults to 10.
#' @param footer_font_family Footer font family. Optional.
#' @param footer_color Footer text color. Optional.
#' @param footer_left Footer left position. Optional.
#' @param footer_top Footer top position. Optional.
#' @param footer_width Footer width. Optional.
#' @param footer_height Footer height. Optional.
#' @param ... Additional arguments reserved for future expansion.
#'
#' @returns
#' Updates the presentation object with the new slide ID, invisibly.
#' @export
add1s <- function(
    presentation = google_presentation,
    layout = "BLANK",
    title = "Title",
    commentary = "Commentary",
    footer = "Footer",
    report_style = c('qualtrics', 'municipal', 'y2'),
    verbose = TRUE,
    convert_slide_size = TRUE,
    slide_size = NULL,
    #Title arguments
    title_font_size = NULL,
    title_font_family = NULL,
    title_font_bold = NULL,
    title_color = NULL,
    title_bg_color = NULL,
    title_left = NULL,
    title_top = NULL,
    title_width = NULL,
    title_height = NULL,
    #Commentary arguments
    commentary_font_size = 14,
    commentary_font_family = NULL,
    commentary_color = NULL,
    commentary_bg_color = NULL,
    commentary_left = NULL,
    commentary_top = NULL,
    commentary_width = NULL,
    commentary_height = NULL,
    #Footer arguments
    footer_font_size = 10,
    footer_font_family = NULL,
    footer_color = NULL,
    footer_left = NULL,
    footer_top = NULL,
    footer_width = NULL,
    footer_height = NULL,
    ...
) {
  report_style <- rlang::arg_match(report_style)

  new_slide(layout = layout, verbose = verbose)

  # Doing Commentary first then title so title will be frontmost.

  add_commentary(
    text = commentary,
    verbose = verbose,
    report_style = report_style,
    commentary_font_size = commentary_font_size,
    commentary_font_family = commentary_font_family,
    commentary_color = commentary_color,
    commentary_bg_color = commentary_bg_color,
    commentary_left = commentary_left,
    commentary_top = commentary_top,
    commentary_width = commentary_width,
    commentary_height = commentary_height,
    convert_slide_size = convert_slide_size,
    slide_size = slide_size
  )

  add_title(
    text = title,
    verbose = verbose,
    report_style = report_style,
    title_font_size = title_font_size,
    title_font_family = title_font_family,
    title_font_bold = title_font_bold,
    title_color = title_color,
    title_bg_color = title_bg_color,
    title_left = title_left,
    title_top = title_top,
    title_width = title_width,
    title_height = title_height,
    convert_slide_size = convert_slide_size,
    slide_size = slide_size
  )

  add_footer(
    text = footer,
    verbose = verbose,
    report_style = report_style,
    footer_font_size = footer_font_size,
    footer_font_family = footer_font_family,
    footer_color = footer_color,
    footer_left = footer_left,
    footer_top = footer_top,
    footer_width = footer_width,
    footer_height = footer_height,
    convert_slide_size = convert_slide_size,
    slide_size = slide_size
  )

  return(invisible())
}
