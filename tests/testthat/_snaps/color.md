# solid_color() errors on invalid input

    Code
      solid_color("NOT_A_COLOR")
    Condition
      Error in `tryCatchList()`:
      ! Invalid color "NOT_A_COLOR". Supply a hex string, a named R color, or an RGB vector.

---

    Code
      solid_color(c(0, 0))
    Condition
      Error in `parse_to_rgb()`:
      ! Numeric color must be `c(r, g, b)` with each value in [0, 1].

---

    Code
      solid_color(c(1.5, 0, 0))
    Condition
      Error in `parse_to_rgb()`:
      ! Numeric color must be `c(r, g, b)` with each value in [0, 1].

# solid_color() errors on invalid alpha

    Code
      solid_color("red", alpha = 1.5)
    Condition
      Error:
      ! <r2slides::solid_color> object properties are invalid:
      - @alpha alpha must be in [0, 1]

---

    Code
      solid_color("red", alpha = c(0.1, 0.2))
    Condition
      Error:
      ! <r2slides::solid_color> object properties are invalid:
      - @alpha alpha must be a single value

# theme_color() errors on invalid theme string

    Code
      theme_color("NOT_A_THEME")
    Condition
      Error:
      ! <r2slides::theme_color> object properties are invalid:
      - @theme theme must be one of: DARK1, LIGHT1, DARK2, LIGHT2, ACCENT1, ACCENT2, ACCENT3, ACCENT4, ACCENT5, ACCENT6, HYPERLINK, FOLLOWED_HYPERLINK, TEXT1, BACKGROUND1, TEXT2, BACKGROUND2

---

    Code
      theme_color(c("ACCENT1", "ACCENT2"))
    Condition
      Error:
      ! <r2slides::theme_color> object properties are invalid:
      - @theme theme must be a single value

# as_opaque_color_api() warns when alpha is set

    Code
      as_opaque_color_api(sc)
    Condition
      Warning:
      Color has alpha set but `opaqueColor` does not support transparency.
      i Alpha will be ignored. Use a fill context if transparency is needed.
    Output
      $opaqueColor
      $opaqueColor$rgbColor
      $opaqueColor$rgbColor$red
      [1] 1
      
      $opaqueColor$rgbColor$green
      [1] 0
      
      $opaqueColor$rgbColor$blue
      [1] 0
      
      
      

