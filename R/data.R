#' Example data
#'
#' A subset of the Washington Soil Health Initiative (WaSHI) State of the Soils
#' Assessment anonymized data. This data set presents each sample in its own
#' row, with columns for each measurement.
#'
#' @format ## `exampleData` A data frame with 100 rows and 51 columns:
#' \describe{
#'   \item{year}{Year of sample}
#'   \item{sampleId, producerId, fieldId}{Anonymized IDs}
#'   \item{farmName, producerName, fieldName}{Anonymized names}
#'   \item{longitude, latitude}{Truncated coordinates}
#'   \item{texture}{Measured soil texture}
#'   \item{other columns}{Column name includes measurement and units; value is the measurement results}
#'   ...
#' }
#' @source [WaSHI State of the
#'   Soils](https://washingtonsoilhealthinitiative.com/state-of-the-soils/)
"exampleData"

#' Data dictionary
#'
#' An example data dictionary for the Washington Soil Health Initiative (WaSHI)
#' State of the Soils Assessment anonymized data.
#'
#' @format ## `dataDictionary` A data frame with 32 rows and 7 columns. columns:
#' \describe{
#'   \item{measurement_group}{Name to group measurements by}
#'   \item{order}{Order of how measurements are presented within each measurement_group}
#'   \item{column_name}{Name of column in data set, used for joining}
#'   \item{measurement_full_name}{Measurement fully spelled out}
#'   \item{abbr}{Abbreviated measurement name for labels}
#'   \item{unit}{Measurement unit}
#'   \item{abbr_unit}{HTML formatted abbreviation with unit for plots and tables}
#'   ...
#' }
#' @source [WaSHI State of the
#'   Soils](https://washingtonsoilhealthinitiative.com/state-of-the-soils/)
"dataDictionary"

#' USDA texture data
#'
#' US Department of Agriculture textural classes
#'
#' @format ## `usdaTexture` A list of two data frames: `polygons` contains 53 rows and 4 columns and `labels` contains 12 rows and 5 columns.
#' \describe{
#'   \item{clay}{Percentage of clay content}
#'   \item{sand}{Percentage of sand content}
#'   \item{silt}{Percentage of silt content}
#'   \item{label}{Texture class}
#'   \item{angle}{Angle to rotate the label in the texture triangle}
#'   ...
#' }
#' @source Adapted from `data(USDA, package = "ggtern")`.
"usdaTexture"
