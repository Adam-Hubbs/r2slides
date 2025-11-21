# Correct slide sizes

Google Slides uses a different default slide size than PowerPoint. In
order to make it easy for the user, and for re-usable code when working
with both Google Slides and PowerPoint, this function converts sizes and
positions that use the PowerPoint system to the Google Slides System.
You may also override the slide_size argument to convert between custom
slide sizes.

## Usage

``` r
correct_slide_size(
  pos,
  dim = c("width", "height"),
  slide_size = NULL,
  call = rlang::caller_env()
)
```

## Arguments

- pos:

  A length one numeric vector.

- dim:

  One of \`"width"\` or \`"height"\`.

- slide_size:

  Optional. A list containing slide size specifications for converting.
  In the form of list(x_height = 7.5, x_width = 13.3, y_height = 5,
  y_width = 9). \`x\_\` indicates converting from and \`y\_\` indicates
  converting too. You can override this to convert between custom slide
  sizes. If left blank it converts from PowerPoint to Google Slides.

- call:

  Optional. The execution environment, used for error messages.

## Value

A numeric value of corrected position/size. Will error if \`slide_size\`
is provided but is not a list.
