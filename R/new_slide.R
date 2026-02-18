
#' Create a new slide in a Google Slides presentation
#'
#' @param presentation Optional. A Google Slides presentation object. 
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
    presentation = get_active_presentation(),
         layout = "BLANK",
    verbose = TRUE,
    master = NULL,
    ...
) {

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
  presentation$refresh()

  # TODO: REturn slide_id object
  invisible()
}

