# Creates a slide_position object with defaults in the title of the slide

Creates a slide_position object with defaults in the title of the slide

## Usage

``` r
in_qualtrics_title(
  revision = Sys.Date(),
  convert_slide_size = FALSE,
  slide_size_old = NULL,
  slide_size = c(5.625, 10)
)
```

## Arguments

- revision:

  A date object to specify the defaults for qualtrics title as of a
  given date

- convert_slide_size:

  A logical. Optional.

- slide_size_old:

  Old Slide size specifications. Optional. Used for conversion between
  powerpoint and google slides sizes

- slide_size:

  New Slide size specifications. Optional.
