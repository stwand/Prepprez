#' Columns for which you need to set a threshold value
#'
#' @param ft - flex table
#' @param col.threshold - Columns for which you need to set a threshold value
#' @param colour.fill.threshold - Threshold fill color
#' @param colour.font.threshold - Threshold font color
#' @param threshold - Threshold value
#' @param inv.threshold - FALSE.Invert colors for thresholds
#'
#' @return Returns a flexible table with specified parameters
#' @export
#'
flex_threshold <- function(
    ft
    ,col.threshold
    ,colour.fill.threshold=c("olivedrab1","pink","grey96")
    ,colour.font.threshold=c("darkgreen","red4","grey3")
    ,threshold=5.0
    ,inv.threshold=FALSE
) {
  my_color_bg <- function(x) {
    out <- rep(colour.fill.threshold[1], length(x))
    if (!inv.threshold) {
      out[x <threshold] <- colour.fill.threshold[2]
    } else {
      out[!(x <threshold)] <- colour.fill.threshold[2]
    }
    out[is.na(x)] <- colour.fill.threshold[3]
    out
  }
  my_color_txt <- function(x) {
    out <- rep(colour.font.threshold[1], length(x))
    if (!inv.threshold) {
      out[x <threshold] <- colour.font.threshold[2]
    } else {
      out[!(x <threshold)] <- colour.font.threshold[2]
    }
    out[is.na(x)] <- colour.font.threshold[3]
    out
  }
  ft <-  flextable::bg(ft,j=col.threshold, bg = my_color_bg)
  ft <- flextable::color(ft,j=col.threshold, color=my_color_txt)
  ft
}
