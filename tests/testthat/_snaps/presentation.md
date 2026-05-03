# get_active_presentation() errors when no active presentation

    Code
      get_active_presentation()
    Condition
      Error in `get_active_presentation()`:
      ! No active presentation

# presentation$get_slide_notes_text() returns notes for slides with and without speaker notes

    Code
      ps$get_slide_notes_text("this_id_does_not_exist")
    Condition
      Error in `ps$get_slide_notes_text()`:
      ! Slide "this_id_does_not_exist" not found in presentation

# presentation$get_slide_by_index() returns the correct slide and errors on bad inputs

    Code
      ps$get_slide_by_index(9999)
    Condition
      Error in `ps$get_slide_by_index()`:
      ! Index out of bounds. Presentation has 7 slides.

---

    Code
      ps$get_slide_by_index(0)
    Condition
      Error in `ps$get_slide_by_index()`:
      ! Index out of bounds. Presentation has 7 slides.

---

    Code
      ps$get_slide_by_index(NULL)
    Condition
      Error in `ps$get_slide_by_index()`:
      ! `index` is required

---

    Code
      ps$get_slide_by_index("a")
    Condition
      Error in `ps$get_slide_by_index()`:
      ! `index` must be a single integer

# presentation$get_slide_by_id() returns the correct slide and errors on bad inputs

    Code
      ps$get_slide_by_id("this_id_does_not_exist")
    Condition
      Error in `ps$get_slide_by_id()`:
      ! Slide ID not found in this presentation

---

    Code
      ps$get_slide_by_id(NULL)
    Condition
      Error in `ps$get_slide_by_id()`:
      ! `slide_id` is required

---

    Code
      ps$get_slide_by_id(c("a", "b"))
    Condition
      Error in `ps$get_slide_by_id()`:
      ! `slide_id` must be a single string

---

    Code
      ps$get_slide_by_id(123)
    Condition
      Error in `ps$get_slide_by_id()`:
      ! `slide_id` must be a single string

# presentation$get_slide_index() returns the 1-based position of a slide

    Code
      ps$get_slide_index("not_a_slide")
    Condition
      Error in `ps$get_slide_index()`:
      ! `slide` must be a slide object

---

    Code
      ps$get_slide_index(NULL)
    Condition
      Error in `ps$get_slide_index()`:
      ! `slide` must be a slide object

# presentation$browse() errors when presentation_id is not set

    Code
      ps$browse()
    Condition
      Error in `ps$browse()`:
      ! Cannot browse: presentation_id is not set

# presentation$print() outputs all fields

    Code
      print(ps)
    Message
      
      -- Google Slides Presentation --
      
      Title: r2slides testing presentation
      ID: 1K9z9yY8Z9qmzOvY-qmO_eNYSpstJvBRObQdnsweaXnY
      Slides: 0
      Active: No
      Last refreshed: <timestamp>
      <https://docs.google.com/presentation/d/1K9z9yY8Z9qmzOvY-qmO_eNYSpstJvBRObQdnsweaXnY>

