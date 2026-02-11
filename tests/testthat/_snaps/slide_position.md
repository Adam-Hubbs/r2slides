# slide_position validates numeric properties

    Code
      slide_position(top = c(1, 2), left = 1, width = 1, height = 1)
    Condition
      Error:
      ! <r2slides::slide_position> object properties are invalid:
      - @top top must be a single value

---

    Code
      slide_position(top = -1, left = 1, width = 1, height = 1)
    Condition
      Error:
      ! <r2slides::slide_position> object properties are invalid:
      - @top top must be greater than or equal to 0

---

    Code
      slide_position(top = 1, left = c(1, 2), width = 1, height = 1)
    Condition
      Error:
      ! <r2slides::slide_position> object properties are invalid:
      - @left left must be a single value

---

    Code
      slide_position(top = 1, left = -1, width = 1, height = 1)
    Condition
      Error:
      ! <r2slides::slide_position> object properties are invalid:
      - @left left must be greater than or equal to 0

---

    Code
      slide_position(top = 1, left = 1, width = c(1, 2), height = 1)
    Condition
      Error:
      ! <r2slides::slide_position> object properties are invalid:
      - @width width must be a single value

---

    Code
      slide_position(top = 1, left = 1, width = 0, height = 1)
    Condition
      Error:
      ! <r2slides::slide_position> object properties are invalid:
      - @width width must be greater than 0

---

    Code
      slide_position(top = 1, left = 1, width = 1, height = c(1, 2))
    Condition
      Error:
      ! <r2slides::slide_position> object properties are invalid:
      - @height height must be a single value

---

    Code
      slide_position(top = 1, left = 1, width = 1, height = 0)
    Condition
      Error:
      ! <r2slides::slide_position> object properties are invalid:
      - @height height must be greater than 0

---

    Code
      slide_position(top = 1, left = 1, width = 1, height = 1, rotation = c(1, 2))
    Condition
      Error:
      ! <r2slides::slide_position> object properties are invalid:
      - @rotation rotation must be a single value

# slide_position validates slide_size properties

    Code
      slide_position(top = 1, left = 1, width = 1, height = 1, slide_size = c(5.625,
        c(10, 20)))
    Condition
      Error in `slide_position()`:
      ! slide_size must be a numeric vector of length 2

---

    Code
      slide_position(top = 1, left = 1, width = 1, height = 1, slide_size = c(5.625,
        0))
    Condition
      Error:
      ! <r2slides::slide_position> object properties are invalid:
      - @slide_width slide_width must be greater than 0

---

    Code
      slide_position(top = 1, left = 1, width = 1, height = 1, slide_size = c(-1, 10))
    Condition
      Error:
      ! <r2slides::slide_position> object properties are invalid:
      - @slide_height slide_height must be greater than 0

# convert_slide_size requires slide_size_old when TRUE

    Code
      slide_position(top = 1, left = 2, width = 3, height = 4, convert_slide_size = TRUE)
    Condition
      Error in `slide_position()`:
      ! slide_size_old must be provided when convert_slide_size is TRUE
      i Provide a numeric vector of length 2: c(height, width)

# convert_slide_size validates input formats

    Code
      slide_position(top = 1, left = 2, width = 3, height = 4, convert_slide_size = TRUE,
        slide_size_old = c(5.625, 10, 15), slide_size = c(7.5, 10))
    Condition
      Error in `slide_position()`:
      ! slide_size_old must be a numeric vector of length 2
      x Got a double vector of length 3
      i Expected format: c(height, width)

---

    Code
      slide_position(top = 1, left = 2, width = 3, height = 4, convert_slide_size = TRUE,
        slide_size_old = "not numeric", slide_size = c(7.5, 10))
    Condition
      Error in `slide_position()`:
      ! slide_size_old must be a numeric vector of length 2
      x Got a string of length 1
      i Expected format: c(height, width)

# mirror validates flip_axis argument

    Code
      mirror(pos, flip_axis = "Invalid")
    Condition
      Error in `method(mirror, r2slides::slide_position)`:
      ! `flip_axis` must be one of "Horizontal" or "Vertical", not "Invalid".

# bounding_box requires at least one object

    Code
      bounding_box()
    Condition
      Error in `bounding_box()`:
      ! At least one slide_position object must be provided

# bounding_box validates all inputs are slide_position

    Code
      bounding_box(pos, "not a position")
    Condition
      Error in `bounding_box()`:
      ! All arguments must be slide_position objects

# bounding_box requires matching slide sizes

    Code
      bounding_box(pos1, pos2)
    Condition
      Error in `bounding_box()`:
      x All slide_position objects must have the same slide size
      i Object 1: 10 x 5.625
      i Object 2: 10 x 7.5

