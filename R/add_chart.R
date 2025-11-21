#' Add a linked Google Sheets chart to a Google Slides presentation
#'
#' @param chart_obj A Google Sheets chart object
#' @param slide_obj A Google Slides slide object
#' @param position An object of class `r2slides::slide_position`
#' @param linked Optional. A logical indicating whether the chart should be linked. Default: `TRUE`.
#' @param token Optional. An OAuth2 token. The default uses `r2slides_token()` to find a token.
#' @param call Optional. Call environment used in error messages.
#'
#' @returns The API response from the batch update request.
#'
#' @export
add_linked_chart <- function(
  chart_obj,
  slide_obj,
  position,
  linked = TRUE,
  token = NULL,
  call = rlang::caller_env()
) {
  linked <- dplyr::if_else(linked, "LINKED", "NOT_LINKED_IMAGE")

  if (!inherits(position, "r2slides::slide_position")) {
    cli::cli_abort(
      "Position must be an object of class {.cls r2slides::slide_position}"
    )
  }

  #TODO Add validation for chart_obj and slide_obj

  left <- position@left_pt
  top <- position@top_pt
  width <- position@width_pt
  height <- position@height_pt

  # Define the request to add a linked chart
  add_linked_chart_request <- list(
    requests = list(
      list(
        createSheetsChart = list(
          elementProperties = list(
            pageObjectId = slide_obj$slide_id,
            size = list(
              width = list(
                magnitude = width,
                unit = "PT"
              ),
              height = list(
                magnitude = height,
                unit = "PT"
              )
            ),
            transform = list(
              scaleX = 1.0,
              scaleY = 1.0,
              translateX = left,
              translateY = top,
              unit = "PT"
            )
          ),
          spreadsheetId = chart_obj$spreadsheet_id,
          chartId = chart_obj$chart_id,
          linkingMode = linked
        )
      )
    )
  )

  # Make the API request
  query2(
    endpoint = 'slides.presentations.batchUpdate',
    params = list(presentationId = slide_obj$presentation_id),
    body = add_linked_chart_request,
    base = 'slides',
    token = token,
    call = call
  )
}
