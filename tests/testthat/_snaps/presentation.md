# get_active_presentation() errors when none is registered

    Code
      get_active_presentation()
    Condition
      Error in `get_active_presentation()`:
      ! No active presentation

# refresh() errors when presentation_id is NULL

    Code
      pres$refresh()
    Condition
      Error in `pres$refresh()`:
      ! Cannot refresh: presentation_id is not set

# get_slide_by_index() errors on out-of-bounds index

    Code
      pres$get_slide_by_index(n + 1L)
    Condition
      Error in `pres$get_slide_by_index()`:
      ! Index out of bounds. Presentation has 2 slides.

# get_slide_by_index() errors on a string input

    Code
      pres$get_slide_by_index("one")
    Condition
      Error in `pres$get_slide_by_index()`:
      ! `index` must be a single integer

# get_slide_by_index() errors on a vector input

    Code
      pres$get_slide_by_index(c(1L, 2L))
    Condition
      Error in `pres$get_slide_by_index()`:
      ! `index` must be a single integer

# get_slide_by_index() errors on NA input

    Code
      pres$get_slide_by_index(NA_integer_)
    Condition
      Error in `pres$get_slide_by_index()`:
      ! `index` must be a single integer

# get_slide_by_id() errors on an unknown ID

    Code
      pres$get_slide_by_id("nonexistent_id")
    Condition
      Error in `pres$get_slide_by_id()`:
      ! Slide ID not found in this presentation

# get_slide_by_id() errors on integer input

    Code
      pres$get_slide_by_id(123L)
    Condition
      Error in `pres$get_slide_by_id()`:
      ! `slide_id` must be a single string

# get_slide_by_id() errors on a character vector input

    Code
      pres$get_slide_by_id(c("a", "b"))
    Condition
      Error in `pres$get_slide_by_id()`:
      ! `slide_id` must be a single string

# browse() errors when presentation_id is NULL

    Code
      pres$browse()
    Condition
      Error in `pres$browse()`:
      ! Cannot browse: presentation_id is not set

# print() produces expected output

    Code
      print(pres)
    Message
      
      -- Google Slides Presentation --
      
      Title: Testing Pres 5
      ID: 1bT5eBiMt1PXcVApTEUWz2cEi820ARpo4NgADGnkF9fE
      Slides: 0
      Active: No
      Last refreshed: <<Last Refreshed Value>>
      <https://docs.google.com/presentation/d/1bT5eBiMt1PXcVApTEUWz2cEi820ARpo4NgADGnkF9fE>

# copy() errors when presentation_id is NULL

    Code
      pres$copy()
    Condition
      Error in `pres$copy()`:
      ! Cannot copy: presentation_id is not set

# delete() errors when presentation_id is NULL

    Code
      pres$delete()
    Condition
      Error in `pres$delete()`:
      ! Cannot delete: presentation_id is not set

