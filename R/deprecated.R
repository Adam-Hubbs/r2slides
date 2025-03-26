
### Helper functions. These are not to be used by users, and will not be exported

### These errors are not working. Use httr2 methods?
#enwSlide is calling with_handling currently
#Infinite loop for authentication problems

#For querying across any scope
query_with_handling <- function(
    url,
    access_token,
    body,
    call = caller_env(),
    ...
) {
  fails <- 0L
  good <- FALSE

  while (fails <= 1 & good == FALSE) {
    tryCatch(
      {
        rsp <- query(
          url = url,
          access_token = access_token,
          body = body,
          call = call,
          ...
        )
        good <- TRUE
        return(rsp)
      },
      error = function(e) {
        if (e$status == 401) {
          cli::cli_alert_warning("Authentication lapsed. Please sign in again.")
          authenticate()
          fails <- fails + 1
        } else {
          cli::cli_abort(
            "Error with request to Google servers",
            parent = e,
            call = call
          )
        }
      }
    )
  }
  return(rsp)
}


query <- function(url, access_token, body, call = caller_env(), ...) {
  request(url) |>
    req_headers("Authorization" = paste("Bearer", access_token)) |>
    req_body_json(body, auto_unbox = TRUE) |>
    req_perform()
}

