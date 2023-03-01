#' calculate mode of categorical variable
#'
#' @param x character vector
#'
#' @export
#'

calculate_mode <- function(x) {
  box::use(stats[na.omit])
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}
