# Style Rule Object

A style rule object is a list of text styles that are applied to a set
of selectors.

## Usage

``` r
style_rule(when = TRUE, what = NULL, default = NULL)
```

## Arguments

- when:

  A list of selection functions that determine when to apply each style.
  Each function should return either TRUE/FALSE (to apply to the whole
  text) or a vector of START_INDEX, END_INDEX (to apply to a specific
  part of the text).

- what:

  A list of text styles. Each style corresponds to a selector in
  \`when\`. If the number of styles is less than the number of
  selectors, the last style is treated as the default style to apply
  when no other selectors match.

- default:

  An optional default text style to apply when no selectors match.
