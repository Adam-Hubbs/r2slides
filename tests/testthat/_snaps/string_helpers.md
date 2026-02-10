# `str_before()` handles errors appropriately

    Code
      str_before("hello", "x")
    Condition
      Error in `str_before()`:
      ! Character "x" not found in text "hello"

---

    Code
      str_before("", ".")
    Condition
      Error in `str_before()`:
      ! Character "." not found in text ""

---

    Code
      str_before("hello", "")
    Condition
      Warning in `stri_locate_first_coll()`:
      empty search patterns are not supported
      Error in `str_before()`:
      ! Character "" not found in text "hello"

---

    Code
      str_before(NULL, ".")
    Condition
      Error in `str_before()`:
      ! Character "." not found in text

---

    Code
      str_before("hello", NULL)
    Condition
      Warning in `stringr::coll()`:
      Coercing `pattern` to a plain character vector.
      Error in `str_before()`:
      ! Character not found in text "hello"

---

    Code
      str_before(123, ".")
    Condition
      Error in `str_before()`:
      ! Character "." not found in text 123

# `str_after()` handles errors appropriately

    Code
      str_after("hello", "x")
    Condition
      Error in `str_after()`:
      ! Character "x" not found in text "hello"

---

    Code
      str_after("", ".")
    Condition
      Error in `str_after()`:
      ! Character "." not found in text ""

---

    Code
      str_after("hello", "")
    Condition
      Warning in `stri_locate_first_coll()`:
      empty search patterns are not supported
      Error in `str_after()`:
      ! Character "" not found in text "hello"

---

    Code
      str_after(NULL, ".")
    Condition
      Error in `str_after()`:
      ! Character "." not found in text

---

    Code
      str_after("hello", NULL)
    Condition
      Warning in `stringr::coll()`:
      Coercing `pattern` to a plain character vector.
      Error in `str_after()`:
      ! Character not found in text "hello"

---

    Code
      str_after(123, ".")
    Condition
      Error in `str_after()`:
      ! Character "." not found in text 123

# `str_matches()` handles errors appropriately

    Code
      str_matches("hello", "xyz")
    Condition
      Error in `str_matches()`:
      ! Pattern {pattern} .val} not found in text {text} .val}

---

    Code
      str_matches("", "test")
    Condition
      Error in `str_matches()`:
      ! Pattern {pattern} .val} not found in text {text} .val}

---

    Code
      str_matches(NULL, "test")
    Condition
      Error in `str_matches()`:
      ! Pattern {pattern} .val} not found in text {text} .val}

---

    Code
      str_matches("hello", NULL)
    Condition
      Error in `stringr::str_locate()`:
      ! `pattern` must be a character vector, not NULL.

---

    Code
      str_matches("hello", "[")
    Condition
      Error in `stri_locate_first_regex()`:
      ! Missing closing bracket on a bracket expression. (U_REGEX_MISSING_CLOSE_BRACKET, context=`[`)

