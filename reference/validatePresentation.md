# Validate presentation environment

Validate presentation environment

## Usage

``` r
validatePresentation(presentation)
```

## Arguments

- presentation:

  A single string naming a presentation environment.

## Value

Nothing. The function will error if the specified environment doesn't
exist in \`.GlobalEnv\` or if it's missing any of the required objects
(\`presentation_id\`, \`slide_ids\`, or \`current_slide_id\`).
