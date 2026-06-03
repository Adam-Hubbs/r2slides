test_that("is.spreadsheet() returns FALSE for non-spreadsheet objects", {
  expect_false(is.spreadsheet(list()))
  expect_false(is.spreadsheet("character"))
  expect_false(is.spreadsheet(NULL))
  expect_false(is.spreadsheet(42L))
  expect_false(is.spreadsheet(data.frame()))
})

test_that("get_active_spreadsheet() errors when no active spreadsheet", {
  old <- if (
    exists("active_spreadsheet", envir = r2slides:::.r2slides_objects)
  ) {
    get("active_spreadsheet", envir = r2slides:::.r2slides_objects)
  } else {
    NULL
  }
  assign("active_spreadsheet", NULL, envir = r2slides:::.r2slides_objects)
  withr::defer(
    assign("active_spreadsheet", old, envir = r2slides:::.r2slides_objects)
  )

  expect_snapshot(error = TRUE, get_active_spreadsheet())
})

test_that("active_spreadsheet_exists() returns FALSE when none active", {
  old <- if (
    exists("active_spreadsheet", envir = r2slides:::.r2slides_objects)
  ) {
    get("active_spreadsheet", envir = r2slides:::.r2slides_objects)
  } else {
    NULL
  }
  assign("active_spreadsheet", NULL, envir = r2slides:::.r2slides_objects)
  withr::defer(
    assign("active_spreadsheet", old, envir = r2slides:::.r2slides_objects)
  )

  expect_false(active_spreadsheet_exists())
})

test_that("register_spreadsheet() opens by direct ID and populates fields", {
  vcr::use_cassette(
    "spreadsheet_register_id",
    match_requests_on = c("method", "uri", "body_json"),
    {
      ss <- register_spreadsheet(id = TEST_SPREADSHEET_ID, set_active = FALSE)
    }
  )

  expect_true(is.spreadsheet(ss))
  expect_equal(ss$spreadsheet_id, TEST_SPREADSHEET_ID)
  expect_type(ss$title, "character")
  expect_s3_class(ss$sheets, "tbl_df")
  expect_true(nrow(ss$sheets) > 0L)
  expect_true(all(c("name", "id") %in% names(ss$sheets)))
})

test_that("register_spreadsheet() opens by full URL", {
  url <- paste0(
    "https://docs.google.com/spreadsheets/d/",
    TEST_SPREADSHEET_ID,
    "/edit"
  )

  vcr::use_cassette(
    "spreadsheet_register_url",
    match_requests_on = c("method", "uri", "body_json"),
    {
      ss <- register_spreadsheet(id = url, set_active = FALSE)
    }
  )

  expect_true(is.spreadsheet(ss))
  expect_equal(ss$spreadsheet_id, TEST_SPREADSHEET_ID)
})

test_that("spreadsheet active state round-trips through .r2slides_objects", {
  vcr::use_cassette(
    "spreadsheet_register_id",
    match_requests_on = c("method", "uri", "body_json"),
    {
      ss <- register_spreadsheet(id = TEST_SPREADSHEET_ID, set_active = TRUE)
    }
  )
  withr::defer(ss$set_not_active())

  expect_true(ss$is_active())
  expect_true(active_spreadsheet_exists())
  expect_identical(get_active_spreadsheet(), ss)

  ss$set_not_active()
  expect_false(ss$is_active())
  expect_false(active_spreadsheet_exists())
})

test_that("spreadsheet print method snapshot", {
  vcr::use_cassette(
    "spreadsheet_register_id",
    match_requests_on = c("method", "uri", "body_json"),
    {
      ss <- register_spreadsheet(id = TEST_SPREADSHEET_ID, set_active = FALSE)
    }
  )

  expect_snapshot(print(ss))
})

test_that("spreadsheet get_url() returns correct URL", {
  vcr::use_cassette(
    "spreadsheet_register_id",
    match_requests_on = c("method", "uri", "body_json"),
    {
      ss <- register_spreadsheet(id = TEST_SPREADSHEET_ID, set_active = FALSE)
    }
  )

  expect_equal(
    ss$get_url(),
    paste0("https://docs.google.com/spreadsheets/d/", TEST_SPREADSHEET_ID)
  )
})
