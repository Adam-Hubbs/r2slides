# Specify a slide by ID or URL

Creates a slide reference object that can be used to target a specific
slide in a Google Slides presentation. Accepts either a slide ID
(numeric) or a full Google Slides URL.

## Usage

``` r
on_slide_number(n)

on_current_slide()

on_newest_slide()

on_slide_id(id)

on_slide_url(id)
```

## Arguments

- n:

  A numeric slide ID

- id:

  A slide identifier. Can be either: - A numeric slide ID - A string
  containing a numeric slide ID - A full Google Slides URL

## Value

A list containing: - \`presentation_id\`: The Google Slides presentation
ID - \`slide_id\`: The specific slide ID within that presentation

## Details

This function requires a presentation context to be available, either in
the calling environment or in the global environment as
\`google_presentation\`. Use \[create_presentation_env_in_global()\] or
register a presentation first.

When a URL is provided, the function validates that the presentation ID
extracted from the URL matches the currently registered presentation.

## Examples

``` r
if (FALSE) { # \dontrun{
# Using a numeric slide ID
slide_ref <- on_slide_id(6)
} # }
```
