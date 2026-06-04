# Color classes

r2slides represents colors as one of two concrete classes, both of which
are subclasses of `r2s_color`:

## Usage

``` r
solid_color(color, alpha = NULL)

theme_color(theme, alpha = NULL)
```

## Arguments

- alpha:

  `NULL` or a number in \[0, 1\]. `NULL` (the default) omits the alpha
  field from API requests, which Google Slides treats as fully opaque.
  Only meaningful in fill contexts; ignored with a warning in text-style
  contexts.

- color:

  Hex string (e.g. `"#4285F4"` or `"#F4A"`), a named R color (e.g.
  `"red"`), or a numeric vector `c(r, g, b)` with each value in \[0,
  1\].

- theme:

  One of `"DARK1"`, `"LIGHT1"`, `"DARK2"`, `"LIGHT2"`,
  `"ACCENT1"`–`"ACCENT6"`, `"HYPERLINK"`, `"FOLLOWED_HYPERLINK"`,
  `"TEXT1"`, `"BACKGROUND1"`, `"TEXT2"`, `"BACKGROUND2"`.

## Details

- `solid_color` — a color with known RGB values. Accepts a hex string, a
  named R color, or a numeric `c(r, g, b)` vector.

- `theme_color` — a reference to a named Google Slides presentation
  theme color. The actual RGB value is not resolved until the slide is
  rendered.
