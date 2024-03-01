#' Example WaSHI data
#'
#' A subset of the Washington Soil Health Initiative (WaSHI) State of the Soils
#' Assessment anonymized data. This data set presents each sample in its own
#' row, with columns for each measurement.
#'
#' @format ## `washi_data` A data frame with 100 rows and 42 columns:
#' \describe{
#'   \item{year}{Year of sample}
#'   \item{sample_id, producer_id, field_id}{Anonymized IDs}
#'   \item{farm_name, field_name}{Anonymized names}
#'   \item{longitude, latitude}{Truncated coordinates}
#'   \item{texture}{Measured soil texture}
#'   \item{other columns}{Column name includes measurement and units;
#'   value is the measurement result}
#'   ...
#' }
#' @source
#' [WaSHI State of the Soils](https://washingtonsoilhealthinitiative.com/state-of-the-soils/)
"washi_data"

#' Data dictionary
#'
#' An example data dictionary for the Washington Soil Health Initiative (WaSHI)
#' State of the Soils Assessment anonymized data.
#'
#' @format ## `data_dictionary` A data frame with 32 rows and 7 columns.
#' \describe{
#'   \item{measurement_group}{Name to group measurements by}
#'   \item{measurement_group_label}{Label of measurement group to be used as
#'   heading}
#'   \item{column_name}{Name of column in data set, used for joining}
#'   \item{order}{Order of how measurements are presented within each
#'   measurement_group}
#'   \item{abbr}{Abbreviated measurement name for labels}
#'   \item{unit}{Measurement unit}
#'   \item{abbr_unit}{HTML formatted abbreviation with unit for plots and
#'   tables}
#'   ...
#' }
#' @source
#' [WaSHI State of the Soils](https://washingtonsoilhealthinitiative.com/state-of-the-soils/)
"data_dictionary"
