#' Add a linked chart form Google Sheets
#'
#' `add_linked_chart()` takes an existing Google Sheets chart and embeds it in a Google Slides presentation.
#' By default, it is *linked*, so updates made to the chart will also update on the presentation.
#'
#' `add_linked_chart()` puts the inputed chart in the exact area of the supplied `slides_position` object.
#' This means that your chart may be re-sized.
#'
#' @param chart_obj A Google Sheets chart object
#' @param slide_obj A Google Slides slide object
#' @param position An object of class `r2slides::slide_position`
#' @param linked Optional. A logical indicating whether the chart should be linked.
#'    * `TRUE` (the default): The chart is linked so updates made in the Google Sheet will also appear on the presentation.
#'    * `FALSE`: Inserts an image of the chart as it currently exists. Will not automatically update.
#' @param order Optional. One of `"front"` or `"back"`. Controls the Z-order of the
#'   created chart element. Default: `"front"`.
#' @param token Optional. An OAuth2 token. The default uses `r2slides_token()` to find a token.
#' @param call Optional. Call environment used in error messages.
#'
#' @returns The Google Slides slide object
#' @examples
#' if(FALSE) {
#' # Assumes that there is a chart already made on the references sheet
#'  chart_data |>
#'    write_gs("Seahawks vs Dolphins Points Per Game") |>
#'    get_chart_id() |>
#'    add_linked_chart(
#'      on_slide_number(
#'        4
#'      ),
#'      in_top_left()
#'   )
#' }
#' @export
add_linked_chart <- function(
  chart_obj,
  slide_obj,
  position,
  linked = TRUE,
  order = c("front", "back"),
  token = NULL,
  call = rlang::caller_env()
) {
  order <- rlang::arg_match(order)
  linked <- dplyr::if_else(linked, "LINKED", "NOT_LINKED_IMAGE")

  if (!inherits(position, "r2slides::slide_position")) {
    cli::cli_abort(
      "Position must be an object of class {.cls r2slides::slide_position}"
    )
  }

  #TODO Add validation for chart_obj and slide_obj

  # Define the request to add a linked chart
  add_linked_chart_request <- list(
    requests = list(
      list(
        createSheetsChart = list(
          elementProperties = list(
            pageObjectId = slide_obj@slide_id,
            size = list(
              width = list(
                magnitude = position@width_emu,
                unit = "EMU"
              ),
              height = list(
                magnitude = position@height_emu,
                unit = "EMU"
              )
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
          ),
          spreadsheetId = chart_obj@spreadsheet_id,
          chartId = as.integer(chart_obj@chart_id),
          linkingMode = linked
        )
      )
    )
  )

  # Make the API request; response contains the new objectId
  rsp <- query(
    endpoint = 'slides.presentations.batchUpdate',
    params = list(presentationId = slide_obj@presentation$presentation_id),
    body = add_linked_chart_request,
    base = 'slides',
    token = token,
    call = call
  )

  # Apply Z-order using the objectId returned by the API
  new_id <- rsp$replies[[1]]$createSheetsChart$objectId
  if (!is.null(new_id)) {
    if (order == 'back') {
      zorder_by_id(
        presentation_id = slide_obj@presentation$presentation_id,
        element_id = new_id,
        operation = resolve_zorder_op(order),
        call = call
      )
    }
  }

  return(invisible(slide_obj))
}
