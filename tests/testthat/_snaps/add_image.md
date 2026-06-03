# mime_type_from_path() errors on unsupported extension

    Code
      mime_type_from_path("doc.pdf")
    Condition
      Error:
      ! Unsupported image format: ".pdf". Supported formats: png, jpg, gif.

---

    Code
      mime_type_from_path("file.bmp")
    Condition
      Error:
      ! Unsupported image format: ".bmp". Supported formats: png, jpg, gif.

---

    Code
      mime_type_from_path("file.svg")
    Condition
      Error:
      ! Unsupported image format: ".svg". Supported formats: png, jpg, gif.

---

    Code
      mime_type_from_path("img.webp")
    Condition
      Error:
      ! Unsupported image format: ".webp". Supported formats: png, jpg, gif.

# resolve_image_source() errors on non-existent file path

    Code
      resolve_image_source("/nonexistent/path/image.png")
    Condition
      Error:
      ! File not found: '/nonexistent/path/image.png'

# resolve_image_source() errors on unsupported input type

    Code
      resolve_image_source(123L)
    Condition
      Error:
      ! `image` must be a ggplot object, a local file path, or a URL.
      x Got <integer>.

---

    Code
      resolve_image_source(list())
    Condition
      Error:
      ! `image` must be a ggplot object, a local file path, or a URL.
      x Got <list>.

---

    Code
      resolve_image_source(c("a", "b"))
    Condition
      Error:
      ! `image` must be a ggplot object, a local file path, or a URL.
      x Got <character>.

# add_image() errors on invalid position before any API call

    Code
      add_image(slide_obj = structure(list(), class = "fake_slide"), image = "https://example.com/img.png",
      position = "not_a_position")
    Condition
      Error in `add_image()`:
      ! Position must be an object of class <r2slides::slide_position>, not <character>.

