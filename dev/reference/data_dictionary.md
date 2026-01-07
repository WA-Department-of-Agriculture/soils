# Data dictionary

An example data dictionary for the Washington Soil Health Initiative
(WaSHI) State of the Soils Assessment anonymized data.

## Usage

``` r
data_dictionary
```

## Format

### `data_dictionary` A data frame with 32 rows and 7 columns.

- measurement_group:

  Name to group measurements by

- measurement_group_label:

  Label of measurement group to be used as heading

- column_name:

  Name of column in data set, used for joining

- order:

  Order of how measurements are presented within each measurement_group

- abbr:

  Abbreviated measurement name for labels

- unit:

  Measurement unit

- abbr_unit:

  HTML formatted abbreviation with unit for plots and tables

## Source

[WaSHI State of the
Soils](https://washingtonsoilhealthinitiative.com/state-of-the-soils/)
