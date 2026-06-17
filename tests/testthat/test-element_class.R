# ── PURE: edge cases that the API cannot easily produce ───────────────────────

test_that("element_class_for returns base element for unknown raw", {
  expect_identical(element_class_for(list(objectId = "e1")), element)
})

test_that("element_class_for handles empty list without error", {
  expect_identical(element_class_for(list()), element)
})

test_that("element_class_for returns shape_element for shape with missing shapeType", {
  raw <- list(objectId = "e2", shape = list())
  expect_identical(element_class_for(raw), shape_element)
})

test_that("element_class_for sheetsChart takes priority over image", {
  raw <- list(
    objectId = "e3",
    sheetsChart = list(spreadsheetId = "abc"),
    image = list(contentUrl = "http://example.com/img.png")
  )
  expect_identical(element_class_for(raw), chart_element)
})


# ── LIVE: element_class_for dispatches correctly against real API shapes ───────

test_that("element_class_for and get_all_elements dispatch correctly for real API shapes", {
  vcr::use_cassette(
    "element_class_dispatch_live",
    match_requests_on = c("method", "uri"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)

      before <- unlist(ps$get_slide_ids())
      new_slide(ps)
      slide_id <- setdiff(unlist(ps$get_slide_ids()), before)[[1]]

      slide_obj <- on_slide_id(slide_id, ps)

      add_text(slide_obj, "Classifier test", in_top_left())
      add_image(
        slide_obj,
        image = "https://www.gstatic.com/webp/gallery/1.jpg",
        position = in_top_right(),
        fit = "fill"
      )
      add_table(slide_obj, make_plain_ft(), test_table_position())

      # Capture the raw pageElements from the API response directly
      slide_fresh <- on_slide_id(slide_id, ps)
      page_elements <- slide_fresh@elements_raw$pageElements

      # Map each raw element to the class element_class_for selects
      classes_from_raw <- purrr::map(page_elements, element_class_for)

      # Get typed element objects via get_all_elements
      all_els <- get_all_elements(slide_fresh)
      el_types <- purrr::map_chr(all_els, element_type)

      delete_slide_raw(ps, slide_id)
    }
  )

  # The real API returns sheetsChart/image/table/shape keys in pageElements.
  # Verify that at least one of each type we created was correctly classified.
  expect_true(
    any(purrr::map_lgl(classes_from_raw, identical, text_element)),
    info = "Expected at least one text_element from real API pageElements"
  )
  expect_true(
    any(purrr::map_lgl(classes_from_raw, identical, image_element)),
    info = "Expected at least one image_element from real API pageElements"
  )
  expect_true(
    any(purrr::map_lgl(classes_from_raw, identical, table_element)),
    info = "Expected at least one table_element from real API pageElements"
  )

  # get_all_elements should return S7 element objects of the right types
  expect_true(length(all_els) >= 3L)
  expect_true("text" %in% el_types)
  expect_true("image" %in% el_types)
  expect_true("table" %in% el_types)

  # Every object in all_els must be an element subclass
  expect_true(all(purrr::map_lgl(all_els, is_element)))

  # TODO: add a chart_element assertion once add_linked_chart() can be called
  # from test infrastructure without a separately-recorded Sheets cassette.
})
