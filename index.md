### r2slides

[![Codecov test
coverage](https://codecov.io/gh/Adam-Hubbs/r2slides/graph/badge.svg)](https://app.codecov.io/gh/Adam-Hubbs/r2slides)
[![R-CMD-check](https://github.com/Adam-Hubbs/r2slides/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Adam-Hubbs/r2slides/actions/workflows/R-CMD-check.yaml)

r2slides is a package designed to create and modify google slides
presentations in R.

To install r2slides, run `pak::pkg_install('Adam-Hubbs/r2slides)`

To get a more in depth understanding of the main systems in r2slides,
look at the articles on the website.

### Example Workflow

``` r
chart_data |>
  write_gs("Seahawks vs Dolphins Points Per Game") |>
  get_chart_id() |>
  add_linked_chart(
    on_slide_number(
      4
    ),
    in_top_left()
  ) |>
  add_text_multi(
    month_change,
    position_base = in_top_left(),
    position = c(
      seahawks_change_mm,
      dolphins_change_mm
    ),
    text_style = change_style,
    stat_sig = attr(month_change, 'stat_sig')
  ) |>
  add_text_multi(
    moe_text,
    position_base = in_top_left(),
    position = c(
      seahawks_point_estimate,
      dolphins_point_estimate
    ),
    text_style = point_estimate_style
  ) |>
  add_text_multi(
    year_change,
    position_base = in_top_left(),
    position = c(
      seahawks_change_yy,
      dolphins_change_yy
    ),
    text_style = change_style,
    stat_sig = attr(year_change, 'stat_sig')
  )
```
