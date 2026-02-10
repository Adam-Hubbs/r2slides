# String helper functions for text positioning

Helper functions to extract position information for text styling and
pattern matching. These functions return position vectors that can be
used with styling rules to format specific parts of text strings.

## Usage

``` r
str_before(
  text,
  character,
  include_boundary = FALSE,
  missing = c("error", "none", "all")
)

str_after(
  text,
  character,
  include_boundary = FALSE,
  missing = c("error", "none", "all")
)

str_matches(text, pattern, missing = c("error", "none", "all"))
```

## Arguments

- text:

  A character string to search in

- character:

  A character string to search for (literal match)

- include_boundary:

  A logical indicating whether to include the position of the boundary
  character

- missing:

  A character string indicating how to handle missing positions. Options
  are "error", "none", or "all".

- pattern:

  A regular expression pattern to search for

## Value

\- \`str_before()\`: A numeric vector of length 2 containing the start
position (1) and the position of the first occurrence of the character -
\`str_after()\`: A numeric vector of length 2 containing the position of
the first occurrence of the character and the end of the string -
\`str_matches()\`: A matrix with start and end positions of the first
match

## Examples

``` r
# Extract positions before and after a dot
str_before("hello.world", ".")
#> [1] 1 5
str_after("hello.world", ".")
#> [1]  7 11

# Find pattern matches
str_matches("hello.world", "ello")
#> [1] 2 5
str_matches("test123", "\\d+")
#> [1] 5 7
```
