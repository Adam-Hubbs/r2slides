

#' Element Object
#'
#' An object representing a Google Slides element/slide/presentation combo
#' @param presentation A presentation object
#' @param slide A slide object
#' @param element_id An element ID
#'
#' @export
element <- S7::new_class(
  "element",
  properties = list(
    presentation = S7::new_property(validator = function(value) {
      if (!is.presentation(value)) {
        return("presentation must be of class `presentation`")
      }
    }),

    slide = S7::new_property(
      validator = function(value) {
        if (!is.slide(value)) {
          return("slide must be of class `slide`")
        }
        if (length(value) != 1) {
          return("slide_id must be a single value")
        }
      }
    ),

    element_id = S7::new_property(
      class_character,
      setter = function(self, value) {
        if (!is.null(self@element_id)) {
          cli::cli_abort("element_id is read-only")
        } else {
          self@element_id <- value
        }
      },
      validator = function(value) {
        if (length(value) != 1) {
          "element_id must be a single value"
        }
      }
    ),

    raw = S7::new_property(
      getter = function(self) {
        self@slide@elements_raw$pageElements |>
          purrr::keep(objectId == self@element_id)
      }
    ),

    type = S7::new_property(
      S7::class_character,
      getter = function(self) {
        self@raw$shape$shapeType
      }
    )
  ),

  constructor = function(presentation, slide, element_id) {
    S7::new_object(
      S7::S7_object(),
      presentation = presentation,
      slide = slide,
      element_id = element_id
    )
  }
)

#' @rdname element
text_element <- S7::new_class("text_element", parent = element, properties = list(
  text = S7::new_property(S7::class_character, getter = function(self) {
    self@raw$shape$text$textElements[[2]]$textRun$content
  })
))

#' Get all elements in a slide
#' 
#' @param slide A slide object
#' 
#' @return A list of element objects
#' 
#' @export
get_all_elements <- function(slide) {
  raw_elems <- slide@elements_raw$pageElements
  purrr::map(raw_elems, function(elem) {
    element(
      presentation = slide@presentation,
      slide = slide,
      element_id = elem$objectId
    )
  })
}

