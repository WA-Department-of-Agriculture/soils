# Summarize samples across the project

Summarize samples across the project

## Usage

``` r
summarize_by_project(results_long)
```

## Arguments

- results_long:

  Dataframe in tidy, long format with columns: `sample_id`,
  `measurement_group`, `abbr`, `value`.

## Value

A data frame summarizing the average values across the project by
`measurement_group` and `abbr`. Includes a "Field or Average" column
and, if present, a `Texture` column.
