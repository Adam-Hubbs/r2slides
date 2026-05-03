request_buffer <- R6::R6Class(
  "request_buffer",
  public = list(
    initialize = function() {
      private$data <- tibble::tibble(
        id = integer(0),
        request = list(),
        time_requested = as.POSIXct(character(0)),
        resource_id = character(0),
        user_call = list()
      )
      private$next_id <- 1L
    },
    add = function(
      endpoint,
      params,
      body,
      base,
      max_tries,
      backoff_base,
      resource_id,
      user_call
    ) {
      new_row <- tibble::tibble(
        id = private$next_id,
        request = list(list(
          endpoint = endpoint,
          params = params,
          body = body,
          base = base,
          max_tries = max_tries,
          backoff_base = backoff_base
        )),
        time_requested = Sys.time(),
        resource_id = as.character(resource_id %||% NA_character_),
        user_call = list(user_call)
      )
      private$next_id <- private$next_id + 1L
      private$data <- dplyr::bind_rows(private$data, new_row)
      invisible(self)
    },
    view = function() private$data,
    remove_request = function(ids) {
      private$data <- dplyr::filter_out(private$data, id %in% ids)
      invisible(self)
    },
    clear = function() {
      private$data <- tibble::tibble(
        id = integer(0),
        request = list(),
        time_requested = as.POSIXct(character(0)),
        resource_id = character(0),
        user_call = list()
      )
      private$next_id <- 1L
      invisible(self)
    }
  ),
  private = list(
    data = NULL,
    next_id = 1L
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
#'   - `id`: integer row identifier
#'   - `request`: a list of query arguments (endpoint, params, body, base, etc.)
#'   - `time_requested`: when the request was buffered
#'   - `resource_id`: the resource identifier from the request params
#'     (`presentationId`, `spreadsheetId`, or `fileId`), or `NA`
#'   - `user_call`: the calling environment captured at buffer time, used for
#'     error attribution on execution
#' @seealso [set_evaluation_strategy()], [execute_requests()]
#' @export
view_request_buffer <- function() {
  .get_request_buffer()$view()
}

#' Clear the request buffer
#'
#' Removes all entries from the internal request buffer. Useful for resetting
#' state between workflows.
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
#' Executes all pending requests in the buffer. Each request is removed from the
#' buffer after it is attempted, whether or not it succeeds. Typically called
#' after accumulating requests with the `"lazy"` evaluation strategy (see
#' [set_evaluation_strategy()]).
#'
#' @param batch_all Logical. If `TRUE` (the default), all pending
#'   `slides.presentations.batchUpdate` requests for the same presentation are
#'   merged into a single API call per presentation. Other request types are
#'   executed individually. If `FALSE`, every buffered request is executed
#'   one at a time in the order it was added.
#'
#' @return `NULL`, invisibly.
#' @seealso [set_evaluation_strategy()], [view_request_buffer()]
#' @export
execute_requests <- function(batch_all = TRUE) {
  rlang::check_bool(batch_all)

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

  if (nrow(data) == 0L) {
    cli::cli_inform("No pending requests to execute.")
    return(invisible(NULL))
  }

  if (batch_all) {
    execute_batched(buf, data)
  } else {
    execute_sequential(buf, data)
  }

  invisible(NULL)
}

execute_sequential <- function(buf, data) {
  purrr::walk(seq_len(nrow(data)), \(i) {
    req <- data$request[[i]]
    user_call <- data$user_call[[i]]
    id <- data$id[[i]]
    tryCatch(
      {
        do.call(query, c(req, list(call = user_call)))
      },
      error = function(e) {
        cli::cli_warn(c(
          "!" = "Request failed.",
          "x" = conditionMessage(e)
        ))
      }
    )
    buf$remove_request(id)
  })
}

execute_batched <- function(buf, data) {
  batch_endpoint <- "slides.presentations.batchUpdate"

  is_batchable <- purrr::map2_lgl(
    data$request,
    data$resource_id,
    \(req, res_id) identical(req$endpoint, batch_endpoint) && !is.na(res_id)
  )

  non_batch_data <- data[!is_batchable, ]
  if (nrow(non_batch_data) > 0L) {
    execute_sequential(buf, non_batch_data)
  }

  batch_data <- data[is_batchable, ]
  if (nrow(batch_data) == 0L) {
    return(invisible(NULL))
  }

  resources <- unique(batch_data$resource_id)
  resources <- resources[!is.na(resources)]

  purrr::walk(resources, \(res_id) {
    res_mask <- batch_data$resource_id == res_id
    res_rows <- batch_data[res_mask, ]

    combined_requests <- purrr::map(res_rows$request, \(r) r$body$requests) |>
      purrr::list_flatten()

    first <- res_rows$request[[1]]
    combined_body <- list(requests = combined_requests)

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
      },
      error = function(e) {
        cli::cli_warn(c(
          "!" = "Batched requests for resource {.val {res_id}} failed.",
          "x" = conditionMessage(e)
        ))
      }
    )
    buf$remove_request(res_rows$id)
  })
}
