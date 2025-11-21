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
  name = NULL,
  url = NULL,
  presentation_id = NULL,
  local = FALSE,
  ...
)
```

## Arguments

- name:

  Optional. A single string. The name of the presentation in Google
  Drive.

- url:

  Optional. A single string. The URL of the Google Slides presentation.

- presentation_id:

  Optional. A single string. The Google Slides presentation ID.

- local:

  Optional. A logical value indicating whether to register the
  presentation in a new environment.

- ...:

  Currently unused; must be empty.

## Value

If \`local = TRUE\`, returns a new environment containing presentation
details. If \`local = FALSE\`, modifies the global environment and
returns invisibly. Will error if more than one identifier is provided or
if no presentation is found.
