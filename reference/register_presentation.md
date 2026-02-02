# Register a Google Slides presentation

Registers an already created google slides presentation so you can
modify it in R. The user supplies either a name, url, or
presentation_id. Supplying multiple will cause the function to error. If
multiple presentation match a given identifier (only possible for name)
the function presents you with a dribble (see the \`googledrive\`
package) and ask which one you want.

## Usage

``` r
register_presentation(
  id = NULL,
  title = "Untitled Presentation",
  set_active = TRUE
)
```

## Arguments

- id:

  Optional. A single string. The Google Slides presentation ID.

- title:

  Optional. A single string. The title of the presentation.

- set_active:

  Optional. A logical value indicating whether to set the presentation
  as the active presentation.

## Value

A Presentation object
