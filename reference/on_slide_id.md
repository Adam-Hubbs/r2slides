# Create a slide object

Creates a slide reference object that can be used to target a specific
slide in a Google Slides presentation. Accepts either a slide ID or a
full Google Slides URL.

## Usage

``` r
on_slide_id(id, ps)

on_slide_url(url, ps)

on_slide_number(n, ps)

on_slide_after(slide, offset = 1, ps)
```

## Arguments

- id:

  A slide identifier. Can be either: - A numeric slide ID - A string
  containing slide ID - A full Google Slides URL

- ps:

  A presentation object

- url:

  A URL pointing to a Google Slides slide

- n:

  A numeric slide ID

- slide:

  A slide object

- offset:

  A position integer - the number of slides after the reference slide.
  Can be negative to return slides before the reference slide

## Value

A slide object

## Examples

``` r
if (FALSE) { # \dontrun{
# Using slide ID
slide_ref <- on_slide_id('f82nannfsl_0')
} # }
```
