# `slide()` requires a valid presentation

    Code
      slide(presentation = list(presentation_id = "test"), slide_id = "g3cf39e8ab47_0_0")
    Condition
      Error:
      ! <r2slides::slide> object properties are invalid:
      - @presentation presentation must be of class `presentation`

# `slide()` requires a single slide_id

    Code
      slide(presentation = ps, slide_id = c("g3cf39e8ab47_0_0", "g3c5ef24c0f9_0_4"))
    Condition
      Error:
      ! <r2slides::slide> object properties are invalid:
      - @slide_id slide_id must be a single value

---

    Code
      slide(presentation = ps, slide_id = character(0))
    Condition
      Error:
      ! <r2slides::slide> object properties are invalid:
      - @slide_id slide_id must be a single value

# `slide()` validates slide_id is character type

    Code
      slide(presentation = ps, slide_id = 123)
    Condition
      Error:
      ! <r2slides::slide> object properties are invalid:
      - @slide_id must be <character>, not <double>

---

    Code
      slide(presentation = ps, slide_id = NULL)
    Condition
      Error:
      ! <r2slides::slide> object properties are invalid:
      - @slide_id must be <character>, not <NULL>

# `on_slide_id()` fails with NULL id

    Code
      on_slide_id(NULL, ps)
    Condition
      Error in `on_slide_id()`:
      ! `id` must be a single value

# `on_slide_id()` fails with NA id

    Code
      on_slide_id(NA, ps)
    Condition
      Error in `on_slide_id()`:
      ! `id` must be a single value

# `on_slide_id()` fails with multiple ids

    Code
      on_slide_id(c("g3cf39e8ab47_0_0", "g3c5ef24c0f9_0_4"), ps)
    Condition
      Error in `on_slide_id()`:
      ! `id` must be a single value

# `on_slide_number()` fails with NULL

    Code
      on_slide_number(NULL, ps)
    Condition
      Error in `on_slide_number()`:
      ! `n` must be a single numeric value

# `on_slide_number()` fails with NA

    Code
      on_slide_number(NA, ps)
    Condition
      Error in `on_slide_number()`:
      ! `n` must be a single numeric value

# `on_slide_number()` fails with non-numeric input

    Code
      on_slide_number("not_a_number", ps)
    Condition
      Error in `on_slide_number()`:
      ! `n` must be a single numeric value

# `on_slide_number()` fails with multiple values

    Code
      on_slide_number(c(1, 2), ps)
    Condition
      Error in `on_slide_number()`:
      ! `n` must be a single numeric value

# `on_slide_after()` errors at beginning of presentation with negative offset

    Code
      on_slide_after(first_slide, offset = -1, ps)
    Condition
      Error in `ps$get_slide_by_index()`:
      ! Index out of bounds. Presentation has 2 slides.

# `on_slide_after()` errors past end of presentation

    Code
      on_slide_after(last_slide, offset = 5, ps)
    Condition
      Error in `ps$get_slide_by_index()`:
      ! Index out of bounds. Presentation has 2 slides.

# `resolve_presentation_id()` fails with non-character input

    Code
      resolve_presentation_id(123)
    Condition
      Error in `resolve_presentation_id()`:
      ! `id` must be a single string

# `resolve_presentation_id()` fails with multiple values

    Code
      resolve_presentation_id(c("id1", "id2"))
    Condition
      Error in `resolve_presentation_id()`:
      ! `id` must be a single string

