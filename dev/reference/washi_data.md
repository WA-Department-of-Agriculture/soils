# Example WaSHI data

A subset of the Washington Soil Health Initiative (WaSHI) State of the
Soils Assessment anonymized data. This data set presents each sample in
its own row, with columns for each measurement.

## Usage

``` r
washi_data
```

## Format

### `washi_data` A data frame with 100 rows and 41 columns:

- year:

  Year of sample

- sample_id, producer_id, field_id:

  Anonymized IDs

- farm_name:

  Anonymized names

- longitude, latitude:

  Truncated coordinates

- texture:

  Measured soil texture

- other columns:

  Column name includes measurement and units; value is the measurement
  result

## Source

[WaSHI State of the
Soils](https://washingtonsoilhealthinitiative.com/state-of-the-soils/)
