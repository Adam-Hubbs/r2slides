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

# get_safe_length() gives a helpful error when a called position function is passed

    Code
      get_safe_length(relative_position())
    Condition
      Error:
      x A relative slide position function call was passed to the position argument. Please pass the name of the function, or a <r2slides::slide_position> object instead.
      i  - Good: position = relative_position
      i  - Bad: position = relative_position()
      Caused by error in `relative_position()`:
      ! argument "position" is missing, with no default

# add_text_multi() errors when argument lengths cannot be recycled

    Code
      add_text_multi(slide_obj = NULL, text = c("A", "B", "C"), position = list(
        in_top_left(), in_top_right()))
    Condition
      Error in `add_text_multi()`:
      ! Cannot recycle arguments to a common length.
      i All arguments must have length 1 or length 3.
      x Invalid lengths: 2

