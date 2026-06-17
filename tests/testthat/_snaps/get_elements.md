# get_elements errors when passed a non-slide object

    Code
      get_elements(list())
    Condition
      Error in `get_elements()`:
      ! `slide` must be a slide object

# get_elements errors on invalid type values

    Code
      get_elements(list(), type = "invalid_type")
    Condition
      Error in `get_elements()`:
      ! `type` contains unknown element type: "invalid_type"
      i Valid types are: "text", "image", "chart", "table", "shape", and "unknown"

---

    Code
      get_elements(list(), type = c("text", "not_a_type"))
    Condition
      Error in `get_elements()`:
      ! `type` contains unknown element type: "not_a_type"
      i Valid types are: "text", "image", "chart", "table", "shape", and "unknown"

# get_elements errors when within is wrong type

    Code
      get_elements(list(), within = "not a position")
    Condition
      Error in `get_elements()`:
      ! `within` must be a <slide_position> or a numeric vector of length 2
      i Expected `c(top, left)` in inches (a point)

---

    Code
      get_elements(list(), within = c(1, 2, 3))
    Condition
      Error in `get_elements()`:
      ! `within` must be a <slide_position> or a numeric vector of length 2
      i Expected `c(top, left)` in inches (a point)

