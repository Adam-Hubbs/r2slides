test_that("`slide()` can be created with valid inputs", {
  slide_id <- "g3cf39e8ab47_0_0"

  vcr::use_cassette(
    "slide_create_valid",
    match_requests_on = c("method", "uri", "body_json"),
    {
      ps <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
      s <- slide(presentation = ps, slide_id = slide_id)
    }
  )

  expect_true(is.slide(s))
  expect_equal(s@slide_id, slide_id)
  expect_true(is.presentation(s@presentation))
  expect_equal(s@presentation$presentation_id, ps$presentation_id)
  rm(ps)
})

test_that("`slide()` requires a valid presentation", {
  expect_snapshot(
    error = TRUE,
    slide(
      presentation = list(presentation_id = "test"),
      slide_id = "g3cf39e8ab47_0_0"
    )
  )
})

test_that("`slide()` requires a single slide_id", {
  vcr::use_cassette("slide_create_multi_id", {
    ps <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
  })

  expect_snapshot(
    error = TRUE,
    slide(
      presentation = ps,
      slide_id = c("g3cf39e8ab47_0_0", "g3c5ef24c0f9_0_4")
    )
  )
  expect_snapshot(
    error = TRUE,
    slide(presentation = ps, slide_id = character(0))
  )
  rm(ps)
})

test_that("`slide()` validates slide_id is character type", {
  vcr::use_cassette("slide_create_bad_type", {
    ps <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
  })

  expect_snapshot(error = TRUE, slide(presentation = ps, slide_id = 123))
  expect_snapshot(error = TRUE, slide(presentation = ps, slide_id = NULL))
  rm(ps)
})

test_that("`slide@slide_hash` is stable and differs across distinct slides", {
  # Each slide() call fetches the slide from the API. vcr records all four
  # interactions in sequence and replays them in order. s1 and s1_copy hit
  # the same endpoint with the same params, so body_json matching lets vcr
  # serve the same recorded response to both.
  vcr::use_cassette(
    "slide_hash",
    match_requests_on = c("method", "uri", "body_json"),
    {
      ps <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
      s1 <- slide(presentation = ps, slide_id = "p")
      s1_copy <- slide(presentation = ps, slide_id = "p")
      s2 <- slide(presentation = ps, slide_id = "g3cf39e8ab47_0_0")
    }
  )

  hash1 <- s1@slide_hash
  hash1_copy <- s1_copy@slide_hash
  hash2 <- s2@slide_hash

  expect_type(hash1, "character")
  expect_equal(hash1, hash1_copy)
  expect_false(hash1 == hash2)
  rm(ps)
})


test_that("`on_slide_id()` works with valid slide ID", {
  slide_id <- "g3cf39e8ab47_0_0"

  vcr::use_cassette(
    "slide_on_id_valid",
    match_requests_on = c("method", "uri", "body_json"),
    {
      ps <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
      result <- on_slide_id(slide_id, ps)
    }
  )

  expect_true(is.slide(result))
  expect_equal(result@slide_id, slide_id)
  rm(ps)
})

test_that("`on_slide_id()` uses active presentation when ps missing", {
  withr::defer({
    if (active_presentation_exists()) get_active_presentation()$set_not_active()
  })

  vcr::use_cassette(
    "slide_on_id_active_pres",
    match_requests_on = c("method", "uri", "body_json"),
    {
      register_presentation(id = "Testing Pres 5", set_active = TRUE)
      result <- on_slide_id("g3cf39e8ab47_0_0")
    }
  )

  expect_true(is.slide(result))
})

test_that("`on_slide_id()` fails with NULL id", {
  vcr::use_cassette("slide_on_id_null", {
    ps <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
  })

  expect_snapshot(error = TRUE, on_slide_id(NULL, ps))
  rm(ps)
})

test_that("`on_slide_id()` fails with NA id", {
  vcr::use_cassette("slide_on_id_na", {
    ps <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
  })

  expect_snapshot(error = TRUE, on_slide_id(NA, ps))
  rm(ps)
})

test_that("`on_slide_id()` fails with multiple ids", {
  vcr::use_cassette("slide_on_id_multi", {
    ps <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
  })

  expect_snapshot(
    error = TRUE,
    on_slide_id(c("g3cf39e8ab47_0_0", "g3c5ef24c0f9_0_4"), ps)
  )
  rm(ps)
})

test_that("`on_slide_url()` extracts slide from valid URL", {
  slide_id <- "g3cf39e8ab47_0_0"

  vcr::use_cassette(
    "slide_on_url_valid",
    match_requests_on = c("method", "uri", "body_json"),
    {
      ps <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
      url <- paste0(
        "https://docs.google.com/presentation/d/",
        ps$presentation_id,
        "/edit#slide=id.",
        slide_id
      )
      result <- on_slide_url(url, ps)
    }
  )

  expect_true(is.slide(result))
  rm(ps)
})


test_that("`on_slide_number()` works with valid numeric index", {
  vcr::use_cassette(
    "slide_on_number_valid",
    match_requests_on = c("method", "uri", "body_json"),
    {
      ps <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
      result <- on_slide_number(2, ps)
    }
  )

  expect_true(is.slide(result))
  expect_equal(result@slide_id, "g3cf39e8ab47_0_0")
  rm(ps)
})

test_that("`on_slide_number()` uses active presentation when ps missing", {
  withr::defer({
    if (active_presentation_exists()) get_active_presentation()$set_not_active()
  })

  vcr::use_cassette(
    "slide_on_number_active_pres",
    match_requests_on = c("method", "uri", "body_json"),
    {
      register_presentation(id = "Testing Pres 5", set_active = TRUE)
      result <- on_slide_number(1)
    }
  )

  expect_true(is.slide(result))
  expect_equal(result@slide_id, "p")
})

test_that("`on_slide_number()` fails with NULL", {
  vcr::use_cassette("slide_on_number_null", {
    ps <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
  })

  expect_snapshot(error = TRUE, on_slide_number(NULL, ps))
  rm(ps)
})

test_that("`on_slide_number()` fails with NA", {
  vcr::use_cassette("slide_on_number_na", {
    ps <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
  })

  expect_snapshot(error = TRUE, on_slide_number(NA, ps))
  rm(ps)
})

test_that("`on_slide_number()` fails with non-numeric input", {
  vcr::use_cassette("slide_on_number_bad_type", {
    ps <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
  })

  expect_snapshot(error = TRUE, on_slide_number("not_a_number", ps))
  rm(ps)
})

test_that("`on_slide_number()` fails with multiple values", {
  vcr::use_cassette("slide_on_number_multi", {
    ps <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
  })

  expect_snapshot(error = TRUE, on_slide_number(c(1, 2), ps))
  rm(ps)
})

test_that("`on_slide_after()` returns slide after reference", {
  vcr::use_cassette(
    "slide_on_after_valid",
    match_requests_on = c("method", "uri", "body_json"),
    {
      ps <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
      ref_slide <- ps$get_slide_by_index(1)
      result <- on_slide_after(ref_slide, offset = 1, ps)
    }
  )

  expect_true(is.slide(result))
  expect_equal(result@slide_id, "g3cf39e8ab47_0_0")
  rm(ps)
})

test_that("`on_slide_after()` works with negative offset", {
  vcr::use_cassette(
    "slide_on_after_negative",
    match_requests_on = c("method", "uri", "body_json"),
    {
      ps <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
      ref_slide <- ps$get_slide_by_index(2)
      result <- on_slide_after(ref_slide, offset = -1, ps)
    }
  )

  expect_true(is.slide(result))
  expect_equal(result@slide_id, "p")
  rm(ps)
})

test_that("`on_slide_after()` uses active presentation when ps missing", {
  withr::defer({
    if (active_presentation_exists()) get_active_presentation()$set_not_active()
  })

  vcr::use_cassette(
    "slide_on_after_active_pres",
    match_requests_on = c("method", "uri", "body_json"),
    {
      register_presentation(id = "Testing Pres 5", set_active = TRUE)
      ref_slide <- get_active_presentation()$get_slide_by_index(1)
      result <- on_slide_after(ref_slide, offset = 1)
    }
  )

  expect_true(is.slide(result))
  expect_equal(result@slide_id, "g3cf39e8ab47_0_0")
})

test_that("`on_slide_after()` errors at beginning of presentation with negative offset", {
  vcr::use_cassette("slide_on_after_edge_start", {
    ps <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
  })

  first_slide <- ps$get_slide_by_index(1)
  expect_snapshot(error = TRUE, on_slide_after(first_slide, offset = -1, ps))
  rm(ps)
})

test_that("`on_slide_after()` errors past end of presentation", {
  vcr::use_cassette("slide_on_after_edge_end", {
    ps <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
  })

  last_slide <- ps$get_slide_by_index(2)
  expect_snapshot(error = TRUE, on_slide_after(last_slide, offset = 5, ps))
  rm(ps)
})

test_that("`resolve_presentation_id()` extracts ID from a full URL", {
  vcr::use_cassette(
    "slide_resolve_id_url",
    match_requests_on = c("method", "uri", "body_json"),
    {
      ps <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
      url <- paste0(
        "https://docs.google.com/presentation/d/",
        ps$presentation_id,
        "/edit"
      )
      result <- resolve_presentation_id(url)
    }
  )

  expect_equal(result, ps$presentation_id)
  rm(ps)
})

test_that("`resolve_presentation_id()` accepts a bare presentation ID", {
  vcr::use_cassette(
    "slide_resolve_id_direct",
    match_requests_on = c("method", "uri", "body_json"),
    {
      ps <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
      result <- resolve_presentation_id(ps$presentation_id)
    }
  )

  expect_equal(result, ps$presentation_id)
  rm(ps)
})

test_that("`resolve_presentation_id()` handles URL with extra query parameters", {
  vcr::use_cassette(
    "slide_resolve_id_url_params",
    match_requests_on = c("method", "uri", "body_json"),
    {
      ps <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
      url <- paste0(
        "https://docs.google.com/presentation/d/",
        ps$presentation_id,
        "/edit?usp=sharing&slide=id.xyz"
      )
      result <- resolve_presentation_id(url)
    }
  )

  expect_equal(result, ps$presentation_id)
  rm(ps)
})

test_that("`resolve_presentation_id()` fails with non-character input", {
  expect_snapshot(error = TRUE, resolve_presentation_id(123))
})

test_that("`resolve_presentation_id()` fails with multiple values", {
  expect_snapshot(error = TRUE, resolve_presentation_id(c("id1", "id2")))
})
