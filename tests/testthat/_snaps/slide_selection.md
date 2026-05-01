# on_slide_with_notes() errors when no slide matches

    Code
      on_slide_with_notes("this_pattern_xyz_does_not_exist_abc123", ps)
    Condition
      Error in `get_active_presentation()`:
      ! No active presentation

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

