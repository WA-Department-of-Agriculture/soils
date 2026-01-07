# Summarize producer's samples with averages by grouping variable

Summarize producer's samples with averages by grouping variable

## Usage

``` r
summarize_by_var(results_long, producer_samples, var)
```

## Arguments

- results_long:

  Dataframe in tidy, long format with columns: `sample_id`, `texture`,
  `measurement_group`, `abbr`, `value`.

- producer_samples:

  Dataframe in tidy, long format with columns: `measurement_group`,
  `abbr`, `value`.

- var:

  Variable to summarize by.
