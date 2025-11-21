# Validate spreadsheet environment

Validate spreadsheet environment

## Usage

``` r
validatespreadsheet(spreadsheet)
```

## Arguments

- spreadsheet:

  A single string naming a spreadsheet environment.

## Value

Nothing. The function will error if the specified environment doesn't
exist in \`.GlobalEnv\` or if it's missing any of the required objects
(\`spreadsheet_id\`, \`sheet_ids\`).
