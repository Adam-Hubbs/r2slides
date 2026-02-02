# Sheet Id class

Sheet Id class

## Usage

``` r
new_sht_id(gs4_sheet, sheet_id, call = rlang::caller_env())

is_sht_id(x)

# S3 method for class 'sht_id'
print(x, ...)
```

## Arguments

- gs4_sheet:

  A googlesheets4 sheet id, or something coercible to one

- sheet_id:

  An integer referencing a sheet id

- call:

  Optional. Call environment used in error messages.

- x:

  An object of class \`sht_id\`

- ...:

  Additional arguments from print
