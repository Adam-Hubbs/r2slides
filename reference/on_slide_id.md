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

on_slide_with_notes(text, exact = TRUE, on_multiple = c("error", "return"), ps)
```

## Arguments

- id:

  A slide identifier. Can be either:

  - A numeric slide ID

  - A string containing slide ID

  - A full Google Slides URL

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

- text:

  A string to search for in slide speaker notes. By default
  `exact = TRUE`, the notes must equal `text` exactly. Pass a
  [`stringr modifier`](https://stringr.tidyverse.org/reference/modifiers.html)
  object (e.g. `stringr::regex(...)`, `stringr::fixed(...)`,
  `stringr::coll(...)` as `text` to use a different matching strategy.

- exact:

  Logical. When `TRUE` (default) the full notes string must equal
  `text`. When `FALSE`, the notes need only *contain* `text` (matched as
  a Perl-compatible regular expression via
  [stringr::regex()](https://stringr.tidyverse.org/reference/modifiers.html).

- on_multiple:

  What to do when multiple slides match:

  - `error`: (default) raises an error listing the matching slide
    numbers

  - `return`: returns all matches as a named list of slide objects
    (names are slide IDs)

## Value

A slide object

## Examples

``` r
if (FALSE) { # \dontrun{
# Using slide ID
slide_ref <- on_slide_id('f82nannfsl_0')
} # }
```
