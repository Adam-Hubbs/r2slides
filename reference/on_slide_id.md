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

on_slide_with_notes(
  text,
  match = c("exact", "regex"),
  on_multiple = c("error", "return"),
  ps
)
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

- text:

  A string to search for in slide speaker notes

- match:

  How to match `text` against notes: `"exact"` requires an identical
  string; `"regex"` treats `text` as a Perl-compatible regular
  expression

- on_multiple:

  What to do when multiple slides match: `"error"` (default) raises an
  error listing the matching slide numbers; `"return"` returns all
  matches as a named list of slide objects (names are slide IDs)

## Value

A slide object

## Examples

``` r
if (FALSE) { # \dontrun{
# Using slide ID
slide_ref <- on_slide_id('f82nannfsl_0')
} # }
```
