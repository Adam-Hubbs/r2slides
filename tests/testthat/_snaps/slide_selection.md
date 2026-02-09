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
      on_slide_id(c("g3c5ef24c0f9_0_0", "g3c5ef24c0f9_0_4"), ps)
    Condition
      Error in `on_slide_id()`:
      ! `id` must be a single value

