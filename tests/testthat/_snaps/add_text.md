# add_text() errors on non-character text

    Code
      add_text(slide_obj, 123, in_top_left())
    Condition
      Error in `add_text()`:
      ! `text` must be a `character`, not a <numeric>

---

    Code
      add_text(slide_obj, "text", list(top = 1))
    Condition
      Error in `add_text()`:
      ! Position must be an object of class <r2slides::slide_position> not a <list>

