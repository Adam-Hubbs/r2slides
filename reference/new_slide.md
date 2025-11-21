# Create a new slide in a Google Slides presentation

Create a new slide in a Google Slides presentation

## Usage

``` r
new_slide(
  presentation = google_presentation,
  layout = "BLANK",
  verbose = TRUE,
  master = NULL,
  ...
)
```

## Arguments

- presentation:

  Optional. A Google Slides presentation object. By default it searches
  for an environment named \`google_presentation\`.

- layout:

  Optional. A character string specifying the slide layout. Default is
  "TITLE_AND_BODY".

- verbose:

  Optional. A logical value indicating whether to print status updates.

- master:

  Currently unused; must be NULL. Reserved for officer compatibility.
  (May drop in future)

- ...:

  Additional arguments reserved for future expansion, currently unused.

## Value

Updates the presentation object with the new slide ID, invisibly. Will
error if layout is not a character string or if master is not NULL.
