zorder_update <- function(element, operation, call = rlang::caller_env()) {
  if (!is_element(element)) {
    cli::cli_abort(
      "{.arg element} must be of class {.cls element}, not {.cls {class(element)}}.",
      call = call
    )
  }

  zorder_by_id(
    presentation_id = element@presentation$presentation_id,
    element_id = element@element_id,
    operation = operation,
    call = call
  )

  invisible(element)
}


# Shared helper — builds and fires the ZOrder batchUpdate using raw IDs.
# Used internally by add_text(), add_text_multi(), and add_linked_chart()
# so they don't need a full element object.
zorder_by_id <- function(
  presentation_id,
  element_id,
  operation,
  call = rlang::caller_env()
) {
  query(
    endpoint = "slides.presentations.batchUpdate",
    params = list(presentationId = presentation_id),
    body = list(
      requests = list(
        list(
          updatePageElementsZOrder = list(
            pageElementObjectIds = list(element_id),
            operation = operation
          )
        )
      )
    ),
    call = call
  )

  invisible(NULL)
}


# Resolves an `order` argument ("front"/"back") to a ZOrder operation string.
resolve_zorder_op <- function(order) {
  switch(order, front = "BRING_TO_FRONT", back = "SEND_TO_BACK")
}


#' Change the Z-order of a page element
#'
#' These four functions adjust the stacking order of a page element on its
#' slide using the Google Slides
#' [UpdatePageElementsZOrderRequest](https://developers.google.com/workspace/slides/api/reference/rest/v1/presentations/request#updatepageelementszorderrequest).
#'
#' @param element An object of class `element`.
#'
#' @returns The `element` object, invisibly.
#'
#' @export
send_to_front <- function(element) {
  zorder_update(element, "BRING_TO_FRONT")
}

#' @rdname send_to_front
#' @export
send_to_back <- function(element) {
  zorder_update(element, "SEND_TO_BACK")
}

#' @rdname send_to_front
#' @export
send_forward <- function(element) {
  zorder_update(element, "BRING_FORWARD")
}

#' @rdname send_to_front
#' @export
send_backward <- function(element) {
  zorder_update(element, "SEND_BACKWARD")
}
