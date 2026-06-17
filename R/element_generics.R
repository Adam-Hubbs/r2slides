# ── get_raw ───────────────────────────────────────────────────────────────────

#' Get the raw API node for an element
#'
#' @description
#' Fetches the raw page-element list from the Google Slides API and returns the
#' entry whose `objectId` matches `x@element_id`. This is the single point of
#' API contact for all element readers; all other getters (`get_position()`,
#' `get_text()`, etc.) call `get_raw()` first and then parse the result
#' locally.
#'
#' The slide's page data is cached with a 15-second TTL by
#' `presentation$get_page_raw()`, so repeated calls within the same session
#' do not necessarily trigger an HTTP request.
#'
#' @param x An `element` object or subclass.
#'
#' @return A named list representing one entry from the Google Slides API
#'   `pageElements` array.
#'
#' @seealso [get_position()], [get_text()], [get_text_style()], [get_image()],
#'   [get_linked_sheet()], [get_table()]
#'
#' @examples
#' if (FALSE) {
#'   ps <- register_presentation(id = "YOUR_PRESENTATION_ID", set_active = FALSE)
#'   slide_obj <- on_slide_number(1, ps)
#'   el <- get_elements(slide_obj, type = "text")[[1]]
#'   raw <- get_raw(el)
#'   names(raw)
#' }
#'
#' @export
get_raw <- S7::new_generic("get_raw", "x")

S7::method(get_raw, element) <- function(x) {
  page_elements <- x@slide@elements_raw$pageElements
  idx <- purrr::detect_index(page_elements, \(el) el$objectId == x@element_id)
  if (idx == 0L) {
    cli::cli_abort(
      "Element {.val {x@element_id}} not found in slide {.val {x@slide@slide_id}}."
    )
  }
  page_elements[[idx]]
}


# ── get_position ──────────────────────────────────────────────────────────────

#' Get the position of an element
#'
#' @description
#' Parses the element's affine transform from the raw API node and returns a
#' `slide_position` object encoding top, left, width, height (all in inches),
#' and rotation (in degrees). Works on any element subclass.
#'
#' Google Slides stores positions using an affine transform matrix
#' (`scaleX`, `scaleY`, `shearX`, `shearY`, `translateX`, `translateY`) plus
#' a nominal size. `get_position()` decomposes the matrix to recover the
#' rendered dimensions and rotation angle.
#'
#' @param x An `element` object or subclass.
#'
#' @return A `slide_position` object. Can be passed directly to `add_text()`,
#'   `add_image()`, etc. to recreate the element at the same location.
#'
#' @seealso [slide_position()], [get_raw()]
#'
#' @examples
#' if (FALSE) {
#'   ps <- register_presentation(id = "YOUR_PRESENTATION_ID", set_active = FALSE)
#'   slide_obj <- on_slide_number(1, ps)
#'   el <- get_elements(slide_obj, type = "text")[[1]]
#'   pos <- get_position(el)
#'   pos@top
#'   pos@left
#' }
#'
#' @export
get_position <- S7::new_generic("get_position", "x")

S7::method(get_position, element) <- function(x) {
  position_from_raw(get_raw(x))
}


# ── get_text ───────────────────────────────────────────────────────────────────

# Concatenate the content of each text run into a single string.
runs_text <- function(runs) {
  paste(runs$content, collapse = "")
}

#' Get the text content of an element
#'
#' @description
#' Concatenates the content of every text run in the element's text body into a
#' single character string. Paragraph markers and other non-run entries are
#' skipped. Only defined for `text_element` objects; calling on any other
#' element type errors with an informative message.
#'
#' @param x A `text_element` object.
#'
#' @return A single character string containing the full text of the element.
#'   An empty string (`""`) is returned if the element has no text runs.
#'
#' @seealso [get_text_style()], [get_raw()]
#'
#' @examples
#' if (FALSE) {
#'   ps <- register_presentation(id = "YOUR_PRESENTATION_ID", set_active = FALSE)
#'   slide_obj <- on_slide_number(1, ps)
#'   el <- get_elements(slide_obj, type = "text")[[1]]
#'   get_text(el)
#' }
#'
#' @export
get_text <- S7::new_generic("get_text", "x")

S7::method(get_text, text_element) <- function(x) {
  # Google Slides appends a trailing "\n " to text box content; trim it so the
  # returned text is the user-visible string and round-trips cleanly through
  # add_text() (avoids a compounding newline and a spurious second text run).
  trimws(runs_text(text_runs(get_raw(x))), which = "right")
}

S7::method(get_text, element) <- function(x) {
  cli::cli_abort(
    "Cannot get text from a {.val {element_type(x)}} element. {.fn get_text} only works on {.cls text_element} objects."
  )
}


# ── get_text_style ────────────────────────────────────────────────────────────

# Collapse per-run text styles into a single text_style (when all runs share
# the same style) or a style_rule with one index-based func selector per run.
#
# Note: only character-level styles are captured here. Paragraph styles live on
# paragraphMarker entries, which text_runs() skips entirely. To read paragraph
# styles you would need to inspect those entries separately.
collapse_run_styles <- function(runs) {
  if (nrow(runs) == 0L) {
    return(text_style())
  }

  styles <- purrr::map(runs$style, \(s) text_style_from_api(style = s))

  # If all runs share an identical style, return that single bare text_style.
  all_same <- all(purrr::map_lgl(styles[-1], \(s) identical(s, styles[[1]])))
  if (length(styles) == 1L || all_same) {
    return(styles[[1]])
  }

  # Build one function per run that returns c(start, end) in 1-based inclusive
  # R indices (create_styling_request converts these to 0-based API indices:
  # startIndex = f_output[1] - 1, endIndex = f_output[2]).
  # Use a factory to capture start/end by value, avoiding loop-closure issues.
  make_range_fn <- function(start, end) {
    force(start)
    force(end)
    function() c(start, end)
  }

  selectors <- purrr::map2(runs$start, runs$end, make_range_fn)

  style_rule(when = list(!!!selectors), what = styles)
}

#' Get the text style of an element
#'
#' @description
#' Inspects every text run in the element and returns a styling object that can
#' be passed directly to `add_text()` via its `text_style` argument:
#'
#' - **Uniform style** (all runs share identical character-level styles):
#'   returns a single `text_style` object.
#' - **Mixed styles** (runs differ in at least one style property): returns a
#'   `style_rule` with one `func`-type selector per run. Each selector function
#'   returns `c(start, end)` 1-based inclusive character indices for that run,
#'   matching the convention expected by `create_styling_request()`.
#'
#' Note: only character-level styles (bold, italic, font size, color, etc.) are
#' captured. Paragraph-level styles (alignment, line spacing, indentation) live
#' on `paragraphMarker` entries, which `text_runs()` skips, so they will not be
#' reflected in the returned object.
#'
#' @param x A `text_element` object.
#'
#' @return A `text_style` object (uniform case) or a `style_rule` object
#'   (mixed-style case). Either can be passed directly to `add_text(text_style =
#'   ...)` to reproduce the styling on a new element.
#'
#' @seealso [get_text()], [text_style()], [style_rule()]
#'
#' @examples
#' if (FALSE) {
#'   ps <- register_presentation(id = "YOUR_PRESENTATION_ID", set_active = FALSE)
#'   slide_obj <- on_slide_number(1, ps)
#'   el <- get_elements(slide_obj, type = "text")[[1]]
#'
#'   sty <- get_text_style(el)
#'   # sty is a text_style for uniform styling or a style_rule for mixed styling
#'
#'   # Recreate the element at the same position with the extracted style:
#'   add_text(
#'     slide_obj,
#'     get_text(el),
#'     get_position(el),
#'     text_style = sty
#'   )
#' }
#'
#' @export
get_text_style <- S7::new_generic("get_text_style", "x")

S7::method(get_text_style, text_element) <- function(x) {
  collapse_run_styles(text_runs(get_raw(x)))
}

S7::method(get_text_style, element) <- function(x) {
  cli::cli_abort(
    "Cannot get text style from a {.val {element_type(x)}} element. {.fn get_text_style} only works on {.cls text_element} objects."
  )
}


# ── get_image ─────────────────────────────────────────────────────────────────

#' Get image URL or file path from an image element
#'
#' @description
#' Extracts the content URL for an image element from the raw API node.
#' By default the URL is returned as a string. Set `download = TRUE` to
#' download the image to a temporary file and return the file path instead.
#'
#' The content URL is a short-lived signed URL issued by Google; it is valid
#' for the duration of the current session but should not be stored permanently.
#'
#' Only defined for `image_element` objects; calling on any other element type
#' errors with an informative message.
#'
#' @param x An `image_element` object.
#' @param download Logical. If `FALSE` (default), returns the content URL
#'   as a character string. If `TRUE`, downloads the image to a temp file
#'   (`.png` extension) and returns the file path.
#'
#' @return A single character string: either the signed content URL
#'   (`download = FALSE`) or an absolute path to a temporary file
#'   (`download = TRUE`).
#'
#' @seealso [get_raw()], [add_image()]
#'
#' @examples
#' if (FALSE) {
#'   ps <- register_presentation(id = "YOUR_PRESENTATION_ID", set_active = FALSE)
#'   slide_obj <- on_slide_number(1, ps)
#'   el <- get_elements(slide_obj, type = "image")[[1]]
#'
#'   # Get the signed URL
#'   url <- get_image(el)
#'
#'   # Download to a temp file
#'   path <- get_image(el, download = TRUE)
#' }
#'
#' @export
get_image <- S7::new_generic("get_image", "x")

S7::method(get_image, image_element) <- function(x, download = FALSE) {
  img <- get_raw(x)$image
  content_url <- img$contentUrl
  if (!download) {
    return(content_url)
  }
  tmp <- tempfile(fileext = ".png")
  utils::download.file(content_url, destfile = tmp, mode = "wb", quiet = TRUE)
  tmp
}

S7::method(get_image, element) <- function(x, download = FALSE) {
  cli::cli_abort(
    "Cannot get image from a {.val {element_type(x)}} element. {.fn get_image} only works on {.cls image_element} objects."
  )
}


# ── get_linked_sheet ──────────────────────────────────────────────────────────

#' Get the linked Google Sheet chart reference from a chart element
#'
#' @description
#' Resolves the chart's `spreadsheetId` and `chartId` from the raw API node
#' back to a `chart_id` object. The function makes one API call to the Google
#' Sheets API to identify which sheet tab within the spreadsheet contains the
#' chart, so the returned `chart_id` carries `spreadsheet_id`, `sheet_id`, and
#' `chart_id`.
#'
#' This is useful for round-tripping: extract the reference from an existing
#' linked chart element and pass it to `add_linked_chart()` to insert it on
#' another slide.
#'
#' Only defined for `chart_element` objects; calling on any other element type
#' errors with an informative message.
#'
#' @param x A `chart_element` object.
#'
#' @return A `chart_id` S7 object with properties `spreadsheet_id`,
#'   `sheet_id`, and `chart_id`.
#'
#' @seealso [get_raw()], [chart_id()]
#'
#' @examples
#' if (FALSE) {
#'   ps <- register_presentation(id = "YOUR_PRESENTATION_ID", set_active = FALSE)
#'   slide_obj <- on_slide_number(1, ps)
#'   el <- get_elements(slide_obj, type = "chart")[[1]]
#'   cid <- get_linked_sheet(el)
#'   cid@spreadsheet_id
#'   cid@chart_id
#' }
#'
#' @export
get_linked_sheet <- S7::new_generic("get_linked_sheet", "x")

S7::method(get_linked_sheet, chart_element) <- function(x) {
  raw_chart <- get_raw(x)$sheetsChart
  spreadsheet_id <- raw_chart$spreadsheetId
  api_chart_id <- raw_chart$chartId

  response <- query(
    endpoint = "sheets.spreadsheets.get",
    params = list(
      spreadsheetId = spreadsheet_id,
      includeGridData = FALSE
    ),
    base = "sheets"
  )

  # Find the sheet tab that contains this chartId.
  resolved_sheet <- purrr::detect(
    response$sheets,
    \(sheet) {
      any(purrr::map_lgl(
        sheet$charts %||% list(),
        \(ch) ch$chartId == api_chart_id
      ))
    }
  )

  if (is.null(resolved_sheet)) {
    cli::cli_abort(
      "Could not find chart {.val {api_chart_id}} in any sheet of spreadsheet {.val {spreadsheet_id}}."
    )
  }

  chart_id(
    spreadsheet_id = spreadsheet_id,
    sheet_id = as.character(resolved_sheet$properties$sheetId),
    chart_id = as.character(api_chart_id)
  )
}

S7::method(get_linked_sheet, element) <- function(x) {
  cli::cli_abort(
    "Cannot get linked sheet from a {.val {element_type(x)}} element. {.fn get_linked_sheet} only works on {.cls chart_element} objects."
  )
}


# ── get_table ─────────────────────────────────────────────────────────────────

# Extract cell text from a raw table node, returning a tibble with columns
# row, col, and text.
# TODO: Richer extraction (cell styles, borders, merged cells) is out of scope
# for this initial implementation; add r2slides_table reconstruction later.
table_text_from_raw <- function(raw) {
  rows <- raw$table$tableRows %||% list()
  if (length(rows) == 0L) {
    return(tibble::tibble(row = integer(), col = integer(), text = character()))
  }

  purrr::imap_dfr(rows, function(row_data, row_i) {
    cells <- row_data$tableCells %||% list()
    purrr::imap_dfr(cells, function(cell, col_i) {
      text_elements <- cell$text$textElements %||% list()
      cell_text <- paste(
        purrr::map_chr(text_elements, function(el) {
          el$textRun$content %||% ""
        }),
        collapse = ""
      )
      tibble::tibble(row = row_i, col = col_i, text = cell_text)
    })
  })
}

#' Get table cell text from a table element
#'
#' @description
#' Extracts the text content from each cell of a Google Slides table element
#' and returns it as a tidy tibble with one row per cell. If a cell contains
#' multiple text runs they are concatenated before being returned.
#'
#' Only defined for `table_element` objects; calling on any other element type
#' errors with an informative message.
#'
#' @details
#' The current implementation captures text only. Cell styles, borders, and
#' merged-cell spans are not yet extracted. Full `r2slides_table`
#' reconstruction is planned for a future release.
#'
#' @param x A `table_element` object.
#'
#' @return A tibble with columns:
#'   - `row` (`integer`): 1-based row index.
#'   - `col` (`integer`): 1-based column index.
#'   - `text` (`character`): concatenated text content of the cell.
#'
#' @seealso [get_raw()], [table_element]
#'
#' @examples
#' if (FALSE) {
#'   ps <- register_presentation(id = "YOUR_PRESENTATION_ID", set_active = FALSE)
#'   slide_obj <- on_slide_number(1, ps)
#'   el <- get_elements(slide_obj, type = "table")[[1]]
#'   tbl <- get_table(el)
#'   tbl
#' }
#'
#' @export
get_table <- S7::new_generic("get_table", "x")

S7::method(get_table, table_element) <- function(x) {
  table_text_from_raw(get_raw(x))
}

S7::method(get_table, element) <- function(x) {
  cli::cli_abort(
    "Cannot get table from a {.val {element_type(x)}} element. {.fn get_table} only works on {.cls table_element} objects."
  )
}
