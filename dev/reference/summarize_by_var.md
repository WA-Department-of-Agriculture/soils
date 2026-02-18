# Summarize producer's samples with averages by grouping variable

Summarize producer's samples with averages by grouping variable

## Usage

``` r
summarize_by_var(results_long, producer_samples, var)
```

## Arguments

- results_long:

  Dataframe in tidy, long format with columns: `sample_id`,
  `measurement_group`, `abbr`, and `value`. If a `texture` column is
  present, the most frequent texture (mode) is included in the output.

- producer_samples:

  Dataframe in tidy, long format with columns: `measurement_group`,
  `abbr`, `value`.

- var:

  Variable to summarize by, which should be present in both
  `results_long` and `producer_samples`.

## Value

A data frame with a "Field or Average" column summarizing the average
values by measurement group, `abbr`, and the specified grouping
variable. If a `texture` column is present, the data frame will contain
a `Texture` column containing the most frequent texture class per
grouping variable.
