# Change the Z-order of a page element

These four functions adjust the stacking order of a page element on its
slide using the Google Slides
\[UpdatePageElementsZOrderRequest\](https://developers.google.com/workspace/slides/api/reference/rest/v1/presentations/request#updatepageelementszorderrequest).

## Usage

``` r
send_to_front(element)

send_to_back(element)

send_forward(element)

send_backward(element)
```

## Arguments

- element:

  An object of class \`element\`.

## Value

The \`element\` object, invisibly.
