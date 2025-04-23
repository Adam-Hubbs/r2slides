#' Set default style values
#'
#' @description
#' This function provides a way to set defaults for all sorts of uses within the themes we
#' established at y2. One can create a new style easily and in one place, and it will update
#' the defaults across all r2slides functions.
#'
#' @param args A named list of style arguments. If you provide a value, it will not be overrided.
#' @param type One of `"title"`, `"commentary"`, or `"footer"`.
#' @param report_style One of `"qualtrics"`, `"municipal"`, or `"y2"`.
#'
#' @returns
#' A list of style arguments with defaults applied based on the `type` and
#' `report_style`. User supplied values will propogate.
#'
#' @export
set_defaults <- function(
    args,
    type = c('title', 'commentary', 'footer'),
    report_style = c('qualtrics', 'municipal', 'y2')
) {
  report_style <- rlang::arg_match(report_style)
  type <- rlang::arg_match(type)

  n <- names(args)

  if (report_style == 'qualtrics') {
    ### Title defaults
    if (type == 'title') {
      args$title_font_size <- args$title_font_size %||% 32
      args$title_font_bold <- args$title_font_bold %||% FALSE
      args$title_font_family <- args$title_font_family %||% 'BentonSans Regular'
      args$title_color <- args$title_color %||% "#000000"
      args$title_bg_color <- args$title_bg_color %||% "#FFFFFF" #Deviation from current Y2artisan value (NULL)
      args$title_left <- args$title_left %||% 0.5
      args$title_top <- args$title_top %||% 0.25
      args$title_width <- args$title_width %||% 10.5
      args$title_height <- args$title_height %||% 0.75
    } else if (type == 'commentary') { ### Commentary defaults
      args$commentary_font_family <- args$commentary_font_family %||%
        'BentonSans Regular'
      args$commentary_color <- args$commentary_color %||% "#000000"
      args$commentary_bg_color <- args$commentary_bg_color %||% "#FFFFFF" #Deviation from current Y2artisan value (NULL)
      args$commentary_left <- args$commentary_left %||% 0.5
      args$commentary_top <- args$commentary_top %||% 1
      args$commentary_width <- args$commentary_width %||% 10.5
      args$commentary_height <- args$commentary_height %||% 0.75
    } else if (type == 'footer') { ### Footer defaults
      args$footer_font_family <- args$footer_font_family %||%
        'BentonSans Regular'
      args$footer_color <- args$footer_color %||% "#000000"
      args$footer_left <- args$footer_left %||% 0.5
      args$footer_top <- args$footer_top %||% 7
      args$footer_width <- args$footer_width %||% 10.5
      args$footer_height <- args$footer_height %||% 0.5
    }
  } else if (report_style == 'municipal') {
    ### Title defaults
    if (type == 'title') {
      args$title_font_size <- args$title_font_size %||% 40
      args$title_font_bold <- args$title_font_bold %||% FALSE
      args$title_font_family <- args$title_font_family %||% 'Flama Medium'
      args$title_color <- args$title_color %||% "#FFFFFF"
      args$title_bg_color <- args$title_bg_color %||% "#9EBCDB"
      args$title_left <- args$title_left %||% 0
      args$title_top <- args$title_top %||% 0
      args$title_width <- args$title_width %||% 13.33
      args$title_height <- args$title_height %||% 0.83
    } else if (type == 'commentary') { ### Commentary defaults
      args$commentary_font_family <- args$commentary_font_family %||%
        'Flama Light'
      args$commentary_color <- args$commentary_color %||% "#FFFFFF"
      args$commentary_bg_color <- args$commentary_bg_color %||% "#9EBCDB"
      args$commentary_left <- args$commentary_left %||% 0
      args$commentary_top <- args$commentary_top %||% 0.83
      args$commentary_width <- args$commentary_width %||% 13.33
      args$commentary_height <- args$commentary_height %||% 0.83
    } else if (type == 'footer') { ### Footer defaults
      args$footer_font_family <- args$footer_font_family %||% 'Flama Light'
      args$footer_color <- args$footer_color %||% "#222222"
      args$footer_left <- args$footer_left %||% 0.24
      args$footer_top <- args$footer_top %||% 7.1
      args$footer_width <- args$footer_width %||% 12.76
      args$footer_height <- args$footer_height %||% 0.4
    }
  } else if (report_style == 'y2') {
    ### Title defaults
    if (type == 'title') {
      args$title_font_size <- args$title_font_size %||% 44
      args$title_font_bold <- args$title_font_bold %||% TRUE
      args$title_font_family <- args$title_font_family %||%
        'Flama Semicondensed Bold'
      args$title_color <- args$title_color %||% "#E8903E"
      args$title_bg_color <- args$title_bg_color %||% "#FFFFFF" #Deviation from current Y2artisan value (NULL)
      args$title_left <- args$title_left %||% 0
      args$title_top <- args$title_top %||% 0.05
      args$title_width <- args$title_width %||% 13.33
      args$title_height <- args$title_height %||% 0.88
    } else if (type == 'commentary') { ### Commentary defaults
      args$commentary_font_family <- args$commentary_font_family %||%
        'Flama Semicondensed Basic'
      args$commentary_color <- args$commentary_color %||% "#767171"
      args$commentary_bg_color <- args$commentary_bg_color %||% "#FFFFFF" #Deviation from current Y2artisan value (NULL)
      args$commentary_left <- args$commentary_left %||% 0.16
      args$commentary_top <- args$commentary_top %||% 1.1
      args$commentary_width <- args$commentary_width %||% 12.82
      args$commentary_height <- args$commentary_height %||% 0.34
    } else if (type == 'footer') { ### Footer defaults
      args$footer_font_family <- args$footer_font_family %||%
        'Flama Semicondensed Basic'
      args$footer_color <- args$footer_color %||% "#A6A6A6"
      args$footer_left <- args$footer_left %||% 0
      args$footer_top <- args$footer_top %||% 7.1 # Deviation from current y2artisan value (7.23) Using 7.23 makes it run off the slide.
      args$footer_width <- args$footer_width %||% 12.46
      args$footer_height <- args$footer_height %||% 0.4 # Deviation from current y2artisan value (0.27) Using 0.27 makes it run off the slide.
    }
  }

  args <- args[n]
  return(args)
}
