# Apply text styling to all elements on a slide that match the selector function Can be used to apply to every text element containing a +, every element containing a -, only the text parts of all elements, etc.

Apply text styling to all elements on a slide that match the selector
function Can be used to apply to every text element containing a +,
every element containing a -, only the text parts of all elements, etc.

## Usage

``` r
style_text_if(selector, style)
```

## Arguments

- selector:

  A function that takes a text element and returns TRUE if it should be
  styled.

- style:

  A list of text styling properties.
