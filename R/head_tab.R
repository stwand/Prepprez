#' Creating a table header
#'
#' @param dates - Data for creating a table
#'
#' @return Table with table header
#' @export
#'
head_tab <- function(dates) {
  data.table::setDT(data.frame(col_keys=names(dates)))
}
