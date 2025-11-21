# Create a new Google Slides presentation

Create a new Google Slides presentation

## Usage

``` r
new_presentation(name, location = NULL, verbose = TRUE, overwrite = FALSE, ...)
```

## Arguments

- name:

  A single string specifying the title of the presentation.

- location:

  Optional. A folder location for where to create the presentation.
  Defaults to your My Drive Home.

- verbose:

  Optional. A logical indicating whether to print status updates. (DEV)

- overwrite:

  Optional. A logical indicating whether to overwrite existing
  presentations. Defaults to FALSE. (TRUE for testing purposes)

- ...:

  Additional arguments reserved for future expansion

## Value

Creates a new Google Slides presentation and modifies the global
environment. Returns invisibly.
