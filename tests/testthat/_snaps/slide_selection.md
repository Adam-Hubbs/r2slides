# on_slide_number() errors with invalid input

    Code
      on_slide_number(NULL, ps)
    Condition
      Error in `on_slide_number()`:
      ! `n` must be a single numeric value

---

    Code
      on_slide_number(NA, ps)
    Condition
      Error in `on_slide_number()`:
      ! `n` must be a single numeric value

---

    Code
      on_slide_number(c(1, 2), ps)
    Condition
      Error in `on_slide_number()`:
      ! `n` must be a single numeric value

---

    Code
      on_slide_number("a", ps)
    Condition
      Error in `on_slide_number()`:
      ! `n` must be a single numeric value

# on_slide_id() errors with invalid input

    Code
      on_slide_id(NULL, ps)
    Condition
      Error in `on_slide_id()`:
      ! `id` must be a single value

---

    Code
      on_slide_id(NA, ps)
    Condition
      Error in `on_slide_id()`:
      ! `id` must be a single value

---

    Code
      on_slide_id(c("a", "b"), ps)
    Condition
      Error in `on_slide_id()`:
      ! `id` must be a single value

# on_slide_with_notes() errors when no slide matches

    Code
      on_slide_with_notes("this_pattern_xyz_does_not_exist_abc123", ps = ps)
    Condition
      Error in `on_slide_with_notes()`:
      ! No slides found whose notes equal "this_pattern_xyz_does_not_exist_abc123"

# on_slide_with_notes() errors on invalid text types

    Code
      on_slide_with_notes(123, ps = ps)
    Condition
      Error in `on_slide_with_notes()`:
      ! `text` must be a single non-NA string

---

    Code
      on_slide_with_notes(NA_character_, ps = ps)
    Condition
      Error in `on_slide_with_notes()`:
      ! `text` must be a single non-NA string

---

    Code
      on_slide_with_notes(c("a", "b"), ps = ps)
    Condition
      Error in `on_slide_with_notes()`:
      ! `text` must be a single non-NA string

# on_slide_with_notes() errors by default when multiple slides match

    Code
      on_slide_with_notes("^Notes", match = "regex", ps = ps)
    Condition
      Error in `on_slide_with_notes()`:
      ! 2 slides found whose notes match "^Notes"
      i Matched slide numbers: 4 and 5
      i Use `on_multiple = 'return'` to retrieve all matches

