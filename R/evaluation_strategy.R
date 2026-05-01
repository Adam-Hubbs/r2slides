RequestBuffer <- R6::R6Class(
  "RequestBuffer",
  public = list(
    initialize = function() {
      private$data <- tibble::tibble(
        request = list(),
        time_requested = as.POSIXct(character(0)),
        tried_to_execute = logical(0),
        execute_succeeded = logical(0),
        presentation = character(0)
      )
    },
    add = function(
      endpoint,
      params,
      body,
      base,
      max_tries,
      backoff_base,
      presentation_id
    ) {
      new_row <- tibble::tibble(
        request = list(list(
          endpoint = endpoint,
          params = params,
          body = body,
          base = base,
          max_tries = max_tries,
          backoff_base = backoff_base
        )),
        time_requested = Sys.time(),
        tried_to_execute = FALSE,
        execute_succeeded = NA,
        presentation = as.character(presentation_id %||% NA_character_)
      )
      private$data <- dplyr::bind_rows(private$data, new_row)
      invisible(self)
    },
    view = function() private$data,
    mark_tried = function(indices) {
      private$data$tried_to_execute[indices] <- TRUE
      invisible(self)
    },
    mark_succeeded = function(indices, succeeded) {
      private$data$execute_succeeded[indices] <- succeeded
      invisible(self)
    },
    clear = function() {
      private$data <- tibble::tibble(
        request = list(),
        time_requested = as.POSIXct(character(0)),
        tried_to_execute = logical(0),
        execute_succeeded = logical(0),
        presentation = character(0)
      )
      invisible(self)
    }
  ),
  private = list(
    data = NULL
  )
)

.get_request_buffer <- function() {
  .r2slides_objects$request_buffer
}

#' Set the request evaluation strategy
#'
#' Controls whether API requests are executed immediately (`"eager"`, the
#' default) or buffered for later batch execution (`"lazy"`). When `"lazy"`,
#' requests accumulate in an internal buffer until [execute_requests()] is
#' called.
#'
#' @param strategy One of `"eager"` (default) or `"lazy"`.
#'
#' @return The strategy string, invisibly.
#' @seealso [get_evaluation_strategy()], [execute_requests()],
#'   [view_request_buffer()]
#' @export
set_evaluation_strategy <- function(strategy = c("eager", "lazy")) {
  strategy <- rlang::arg_match(strategy)
  Sys.setenv(r2slides_request_evaluation_strategy = strategy)
  invisible(strategy)
}

#' Get the current request evaluation strategy
#'
#' Returns the value of the `r2slides_request_evaluation_strategy` environment
#' variable, defaulting to `"eager"` if unset.
#'
#' @return A string, either `"eager"` or `"lazy"`.
#' @seealso [set_evaluation_strategy()]
#' @export
get_evaluation_strategy <- function() {
  strategy <- Sys.getenv(
    "r2slides_request_evaluation_strategy",
    unset = "eager"
  )
  if (!strategy %in% c("eager", "lazy")) {
    cli::cli_warn(c(
      "!" = "Unknown evaluation strategy {.val {strategy}}; defaulting to {.val eager}.",
      "i" = "Use {.fn set_evaluation_strategy} to set a valid strategy."
    ))
    return("eager")
  }
  strategy
}

#' View the pending request buffer
#'
#' Returns a tibble showing all buffered API requests. Each row represents one
#' deferred [query()] call. Useful for inspecting what is queued when using the
#' `"lazy"` evaluation strategy.
#'
#' @return A [tibble::tibble()] with columns:
#'   - `request`: a list of query arguments (endpoint, params, body, base, etc.)
#'   - `time_requested`: when the request was buffered
#'   - `tried_to_execute`: whether execution has been attempted
#'   - `execute_succeeded`: `NA` if not yet attempted, `TRUE`/`FALSE` otherwise
#'   - `presentation`: the `presentationId` from the request params, or `NA`
#' @seealso [set_evaluation_strategy()], [execute_requests()]
#' @export
view_request_buffer <- function() {
  .get_request_buffer()$view()
}

#' Clear the request buffer
#'
#' Removes all entries from the internal request buffer, including any that have
#' already been executed. Useful for resetting state between workflows.
#'
#' @return `NULL`, invisibly.
#' @seealso [view_request_buffer()], [execute_requests()]
#' @export
clear_request_buffer <- function() {
  .get_request_buffer()$clear()
  invisible(NULL)
}

#' Execute buffered API requests
#'
#' Executes all pending requests in the buffer (those with
#' `tried_to_execute == FALSE`). Typically called after accumulating requests
#' with the `"lazy"` evaluation strategy (see [set_evaluation_strategy()]).
#'
#' @param batch_all Logical. If `TRUE` (the default), all pending
#'   `slides.presentations.batchUpdate` requests for the same presentation are
#'   merged into a single API call per presentation. Other request types are
#'   executed individually. If `FALSE`, every buffered request is executed
#'   one at a time in the order it was added.
#'
#' @return `NULL`, invisibly. The request buffer is updated in place to record
#'   which requests were attempted and whether they succeeded.
#' @seealso [set_evaluation_strategy()], [view_request_buffer()]
#' @export
execute_requests <- function(batch_all = TRUE) {
  # Force eager during execution so buffered calls don't re-buffer themselves
  old_strategy <- Sys.getenv(
    "r2slides_request_evaluation_strategy",
    unset = "eager"
  )
  on.exit(
    Sys.setenv(r2slides_request_evaluation_strategy = old_strategy),
    add = TRUE
  )
  Sys.setenv(r2slides_request_evaluation_strategy = "eager")

  buf <- .get_request_buffer()
  data <- buf$view()
  pending_idx <- which(!data$tried_to_execute)

  if (length(pending_idx) == 0L) {
    cli::cli_inform("No pending requests to execute.")
    return(invisible(NULL))
  }

  if (batch_all) {
    execute_batched(buf, data, pending_idx)
  } else {
    execute_sequential(buf, data, pending_idx)
  }

  invisible(NULL)
}

execute_sequential <- function(buf, data, pending_idx) {
  purrr::walk(pending_idx, \(i) {
    req <- data$request[[i]]
    buf$mark_tried(i)
    tryCatch(
      {
        do.call(query, req)
        buf$mark_succeeded(i, TRUE)
      },
      error = function(e) {
        buf$mark_succeeded(i, FALSE)
        cli::cli_warn(c(
          "!" = "Request {i} failed.",
          "x" = conditionMessage(e)
        ))
      }
    )
  })
}

execute_batched <- function(buf, data, pending_idx) {
  pending <- data[pending_idx, ]
  batch_endpoint <- "slides.presentations.batchUpdate"

  is_batchable <- purrr::map2_lgl(
    pending$request,
    pending$presentation,
    \(req, pres) identical(req$endpoint, batch_endpoint) && !is.na(pres)
  )

  non_batch_idx <- pending_idx[!is_batchable]
  if (length(non_batch_idx) > 0L) {
    execute_sequential(buf, data, non_batch_idx)
  }

  batch_pending_idx <- pending_idx[is_batchable]
  batch_pending <- pending[is_batchable, ]

  if (nrow(batch_pending) == 0L) {
    return(invisible(NULL))
  }

  presentations <- unique(batch_pending$presentation)
  presentations <- presentations[!is.na(presentations)]

  purrr::walk(presentations, \(pres_id) {
    pres_mask <- batch_pending$presentation == pres_id
    pres_global_idx <- batch_pending_idx[pres_mask]
    pres_reqs <- batch_pending$request[pres_mask]

    combined_requests <- purrr::map(pres_reqs, \(r) r$body$requests) |>
      purrr::list_flatten()

    first <- pres_reqs[[1]]
    combined_body <- list(requests = combined_requests)

    buf$mark_tried(pres_global_idx)
    tryCatch(
      {
        query(
          endpoint = first$endpoint,
          params = first$params,
          body = combined_body,
          base = first$base,
          max_tries = first$max_tries,
          backoff_base = first$backoff_base
        )
        buf$mark_succeeded(pres_global_idx, TRUE)
      },
      error = function(e) {
        buf$mark_succeeded(pres_global_idx, FALSE)
        cli::cli_warn(c(
          "!" = "Batched requests for presentation {.val {pres_id}} failed.",
          "x" = conditionMessage(e)
        ))
      }
    )
  })
}
