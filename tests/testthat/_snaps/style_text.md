# validate_color() returns an error string for invalid inputs

    Code
      validate_color(c("r", "g", "b"))
    Output
      [1] "Color must be a length-1 character string or a numeric `c(r, g, b)` vector."

---

    Code
      validate_color(c(-0.1, 0, 0))
    Output
      [1] "Numeric color must be `c(r, g, b)` with each value in [0, 1]."

---

    Code
      validate_color(c(0, 1.1, 0))
    Output
      [1] "Numeric color must be `c(r, g, b)` with each value in [0, 1]."

---

    Code
      validate_color("NOT_A_THEME")
    Output
      [1] "Invalid color \"NOT_A_THEME\". Supply a hex string, a named R color, or a theme color."

---

    Code
      validate_color(c(0.1, 0.2))
    Output
      [1] "Numeric color must be `c(r, g, b)` with each value in [0, 1]."

---

    Code
      validate_color(c(0.1, 0.2, 0.3, 0.4))
    Output
      [1] "Numeric color must be `c(r, g, b)` with each value in [0, 1]."

# text_style() rejects invalid logical vector properties

    Code
      text_style(bold = c(TRUE, FALSE))
    Condition
      Error:
      ! <r2slides::text_style> object properties are invalid:
      - @bold bold must be a single value

---

    Code
      text_style(italic = c(TRUE, FALSE))
    Condition
      Error:
      ! <r2slides::text_style> object properties are invalid:
      - @italic italic must be a single value

---

    Code
      text_style(small_caps = c(TRUE, FALSE))
    Condition
      Error:
      ! <r2slides::text_style> object properties are invalid:
      - @small_caps small_caps must be a single value

---

    Code
      text_style(strikethrough = c(TRUE, FALSE))
    Condition
      Error:
      ! <r2slides::text_style> object properties are invalid:
      - @strikethrough strikethrough must be a single value

---

    Code
      text_style(underline = c(TRUE, FALSE))
    Condition
      Error:
      ! <r2slides::text_style> object properties are invalid:
      - @underline underline must be a single value

# text_style() rejects invalid font_family values

    Code
      text_style(font_family = "")
    Condition
      Error:
      ! <r2slides::text_style> object properties are invalid:
      - @font_family font_family can't be an empty string

---

    Code
      text_style(font_family = c("Arial", "Times"))
    Condition
      Error:
      ! <r2slides::text_style> object properties are invalid:
      - @font_family font_family must be a single value

# text_style() rejects invalid font_size values

    Code
      text_style(font_size = 0)
    Condition
      Error:
      ! <r2slides::text_style> object properties are invalid:
      - @font_size font_size must be greater than 0

---

    Code
      text_style(font_size = -5)
    Condition
      Error:
      ! <r2slides::text_style> object properties are invalid:
      - @font_size font_size must be greater than 0

---

    Code
      text_style(font_size = c(12, 14))
    Condition
      Error:
      ! <r2slides::text_style> object properties are invalid:
      - @font_size font_size must be a single value

# text_style() validates link format

    Code
      text_style(link = "ftp://example.com")
    Condition
      Error:
      ! <r2slides::text_style> object properties are invalid:
      - @link link must be a url

---

    Code
      text_style(link = "example.com")
    Condition
      Error:
      ! <r2slides::text_style> object properties are invalid:
      - @link link must be a url

---

    Code
      text_style(link = "")
    Condition
      Error:
      ! <r2slides::text_style> object properties are invalid:
      - @link link can't be an empty string

# text_style() validates baseline_offset values

    Code
      text_style(baseline_offset = "BASELINE")
    Condition
      Error:
      ! <r2slides::text_style> object properties are invalid:
      - @baseline_offset baseline_offset must be either 'SUPERSCRIPT' or 'SUBSCRIPT'

---

    Code
      text_style(baseline_offset = "superscript")
    Condition
      Error:
      ! <r2slides::text_style> object properties are invalid:
      - @baseline_offset baseline_offset must be either 'SUPERSCRIPT' or 'SUBSCRIPT'

---

    Code
      text_style(baseline_offset = "")
    Condition
      Error:
      ! <r2slides::text_style> object properties are invalid:
      - @baseline_offset baseline_offset must be either 'SUPERSCRIPT' or 'SUBSCRIPT'

# text_style() rejects invalid color values

    Code
      text_style(bg_color = c(-1, 0, 0))
    Condition
      Error in `normalize_color()`:
      ! Numeric color must be `c(r, g, b)` with each value in [0, 1].

---

    Code
      text_style(bg_color = "FAKE_COLOR")
    Condition
      Error in `tryCatchList()`:
      ! Invalid color "FAKE_COLOR". Supply a hex string, a named R color, or a theme color.

---

    Code
      text_style(text_color = c(0, 0, 2))
    Condition
      Error in `normalize_color()`:
      ! Numeric color must be `c(r, g, b)` with each value in [0, 1].

# combine_style_impl() errors on contradicting field values for all overlapable fields

    Code
      combine_style_impl(.x[[1]], .x[[2]], error_on_contradiction = TRUE)
    Condition
      Error in `.f()`:
      ! Can't combine text styles: conflicting values for backgroundColor
      x Object 1 has "#FF0000"
      x Object 2 has "#00FF00"

---

    Code
      combine_style_impl(.x[[1]], .x[[2]], error_on_contradiction = TRUE)
    Condition
      Error in `.f()`:
      ! Can't combine text styles: conflicting values for foregroundColor
      x Object 1 has "#FF0000"
      x Object 2 has "#0000FF"

---

    Code
      combine_style_impl(.x[[1]], .x[[2]], error_on_contradiction = TRUE)
    Condition
      Error in `.f()`:
      ! Can't combine text styles: conflicting values for bold
      x Object 1 has TRUE
      x Object 2 has FALSE

---

    Code
      combine_style_impl(.x[[1]], .x[[2]], error_on_contradiction = TRUE)
    Condition
      Error in `.f()`:
      ! Can't combine text styles: conflicting values for italic
      x Object 1 has TRUE
      x Object 2 has FALSE

---

    Code
      combine_style_impl(.x[[1]], .x[[2]], error_on_contradiction = TRUE)
    Condition
      Error in `.f()`:
      ! Can't combine text styles: conflicting values for fontFamily
      x Object 1 has "A"
      x Object 2 has "B"

---

    Code
      combine_style_impl(.x[[1]], .x[[2]], error_on_contradiction = TRUE)
    Condition
      Error in `.f()`:
      ! Can't combine text styles: conflicting values for fontSize
      x Object 1 has 10
      x Object 2 has 12

---

    Code
      combine_style_impl(.x[[1]], .x[[2]], error_on_contradiction = TRUE)
    Condition
      Error in `.f()`:
      ! Can't combine text styles: conflicting values for link
      x Object 1 has "https://a.com"
      x Object 2 has "https://b.com"

---

    Code
      combine_style_impl(.x[[1]], .x[[2]], error_on_contradiction = TRUE)
    Condition
      Error in `.f()`:
      ! Can't combine text styles: conflicting values for baselineOffset
      x Object 1 has "SUPERSCRIPT"
      x Object 2 has "SUBSCRIPT"

---

    Code
      combine_style_impl(.x[[1]], .x[[2]], error_on_contradiction = TRUE)
    Condition
      Error in `.f()`:
      ! Can't combine text styles: conflicting values for smallCaps
      x Object 1 has TRUE
      x Object 2 has FALSE

---

    Code
      combine_style_impl(.x[[1]], .x[[2]], error_on_contradiction = TRUE)
    Condition
      Error in `.f()`:
      ! Can't combine text styles: conflicting values for strikethrough
      x Object 1 has TRUE
      x Object 2 has FALSE

---

    Code
      combine_style_impl(.x[[1]], .x[[2]], error_on_contradiction = TRUE)
    Condition
      Error in `.f()`:
      ! Can't combine text styles: conflicting values for underline
      x Object 1 has TRUE
      x Object 2 has FALSE

# combine_style() with .priority = 'error' errors on contradictions

    Code
      combine_style(text_style(bold = TRUE), text_style(bold = FALSE), .priority = "error")
    Condition
      Error in `combine_style()`:
      ! Can't combine text styles: conflicting values for bold
      x Object 1 has TRUE
      x Object 2 has FALSE

# combine_style() errors when non-text_style objects are passed

    Code
      combine_style(text_style(bold = TRUE), "not a style")
    Condition
      Error in `combine_style()`:
      x All arguments passed in ... must be text_style objects

# style_rule() errors without what or default

    Code
      style_rule()
    Condition
      Error:
      ! Must provide either `what` or `default`

# style_rule() errors on mismatched selector/style counts

    Code
      style_rule(when = c(grepl("foo", text), grepl("bar", text)), what = text_style(
        bold = TRUE))
    Condition
      Error:
      ! Number of selectors and styles must match.
      i 2 selectors supplied but 1 style in `what`.
      i Use `default` to supply a fallback style applied when no selector matches.

# style_rule() errors when multiple selectors provided with only default

    Code
      style_rule(when = c(grepl("foo", text), grepl("bar", text)), default = text_style(
        bold = TRUE))
    Condition
      Error:
      ! Multiple selectors supplied but no `what`.
      i Provide a style in `what` for each selector, or use a single `default`.

# style_rule() errors when explicit selector given with default but no what

    Code
      style_rule(when = grepl("foo", text), default = text_style(bold = TRUE))
    Condition
      Error:
      ! A selector was provided with `default` but no `what`.
      i Provide a style in `what` for each selector, or remove `when` to use `default` alone.

# create_styling_request() errors on unnamed extra vars

    Code
      create_styling_request(sr, "hello", "e1", "unnamed")
    Condition
      Error:
      ! Extra variables supplied to create_styling_request must be named.

# create_styling_request() gives informative error on missing variable

    Code
      create_styling_request(sr, "text", "e1")
    Condition
      Error:
      x Variable `missing_var` not found when evaluating selector function `missing_var > 10`.
      i Add `missing_var` to the `add_text()` call to make it available to the selector function.

# `create_styling_request()` warns and skips on NA selector result

    Code
      result <- create_styling_request(sr, "text", "e1")
    Condition
      Warning:
      i Selector function returned `NA`. Not preforming any styling.

