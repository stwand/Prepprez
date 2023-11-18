#' Resizes flex table to fit powerpoint dimensions
#'
#' @param ft - flex table
#' @param plan.wid - Planned width in centimeters
#' @param plan.hei - Planned height in centimeters
#' @param col.no - Names of columns, except text ones, for which you do not need to resize. Text columns are not resized by default
#' @param al.w - FALSE. Automatic alignment of columns, except text and selected ones
#' @param cpt - Additional row height scale option. Default is 1.
#'
#' @return Returns a flexible table adjusted to the specified dimensions
#' @export
#'
resize_flex <- function(ft,plan.wid=12,plan.hei=14,col.no=NULL
                        ,al.w=FALSE
                        ,cpt=1) {
  plan.wid <- plan.wid/2.54
  if ((plan.wid/sum(dim(ft)$widths))>=1) {

    size_wid <- dim(ft)$widths
    if (al.w) {
      filt_1 <- unlist(lapply(ft$body$dataset,\(x) is.factor(x)|is.character(x)))
      size_wid[!filt_1] <- 1
      size_wid[!filt_1] <- size_wid[!filt_1]/sum(size_wid[!filt_1])
      size_wid[!filt_1] <- size_wid[!filt_1]*(plan.wid-sum(size_wid[filt_1]))
    } else {
      size_wid <- plan.wid/sum(dim(ft)$widths)*dim(ft)$widths
    }
  } else {
    filt_1 <- unlist(lapply(ft$body$dataset,\(x) is.factor(x)|is.character(x))) |
      (names(ft$body$dataset) %in% col.no)
    size_wid <- dim(ft)$widths
    if (al.w) {size_wid[!filt_1] <- 1}
    size_wid[!filt_1] <- size_wid[!filt_1]/sum(size_wid[!filt_1])
    size_wid[!filt_1] <- size_wid[!filt_1]*(plan.wid-sum(size_wid[filt_1]))
  }
  ft <- flextable::width(ft, width = size_wid)
  ft <- flextable::hrule(ft, rule = "exact", part = "all")
  plan.hei <- plan.hei/2.54
  size_hei <- plan.hei/sum(dim(ft)$heights)*dim(ft)$heights
  head_n <- (length(ft$header$hrule))
  ft <- flextable::height(ft, height = size_hei[1:head_n], part = "header")
  ft <- flextable::height(ft, height = size_hei[(head_n+1):length(size_hei)], part = "body")
  if (!(cpt==1)) {
    ft <- flextable::height_all(ft,height = mean(dim(ft)$heights)*cpt,part = "body")
  }
  ft
}
