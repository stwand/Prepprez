#' Returns a flexible table with specified parameters
#'
#' @param dates - Data frame
#' @param head.df - Table header in the form of a data frame
#' @param font.size.header - Header font size
#' @param font.size.body - Body font size
#' @param font.size.total - Total font size
#' @param colour.smoll.bord - Inner border color
#' @param colour.big.border - Outer border color
#' @param colour.head.fill - Table header color
#' @param colour.head.text Table header color font
#' @param colour.total.fill -Table total color
#' @param nm.total - The word used to mark the total lines
#' @param col.total - Column in which to look for total lines
#' @param col.lv1 - Select a color for the topmost line of a multi-level header
#' @param lv1.align - If you change the color of the top line of the header, use alignment
#' @param col.style.prc - Columns in which a percent sign is added to the values
#' @param col.threshold - Columns for which you need to set a threshold value
#' @param colour.fill.threshold - Threshold fill color
#' @param colour.font.threshold - Threshold font color
#' @param threshold - Threshold value
#' @param inv.threshold - FALSE.Invert colors for thresholds
#' @param font.nm -  Font family
#' @param padd - padding
#' @param bold.head - TRUE. Table header in bold
#' @param bold.body - FALSE. Table body in bold
#' @param bold.total - TRUE. Table total in bold
#'
#' @return Returns a flexible table with specified parameters
#' @export
#'
flex_form_tab <- function(
    dates
    ,head.df
    ,font.size.header=10
    ,font.size.body=10
    ,font.size.total=10
    ,colour.smoll.bord="gray2"
    ,colour.big.border="gray4"
    ,colour.head.fill="gray"
    ,colour.head.text="black"
    ,colour.total.fill="gray"
    ,nm.total="Total"
    ,col.total=1
    ,col.lv1=NULL #
    ,lv1.align="left"
    ,col.style.prc=NULL
    ,col.threshold=NULL
    ,colour.fill.threshold=c("olivedrab1","pink","grey96")
    ,colour.font.threshold=c("darkgreen","red4","grey3")
    ,threshold=5.0
    ,inv.threshold=FALSE
    ,font.nm="Century Schoolbook"
    ,padd=0
    ,bold.head=TRUE
    ,bold.body=FALSE
    ,bold.total=TRUE
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
  small_border <-  officer::fp_border(color=colour.smoll.bord, width = 0.5)
  big_border <-  officer::fp_border(color=colour.big.border, width = 1)
  ft <- dates |>
    flextable::flextable() |>
    flextable::padding(padding=padd) |>
    flextable::theme_vanilla() |>
    flextable::set_header_df(mapping = head.df, key = "col_keys" ) |>
    flextable::border_inner_h(part="all", border = small_border ) |>
    flextable::border_inner_v(part="all", border = small_border ) |>
    flextable::border_outer(part="all", border = big_border) |>
    flextable::merge_h(part = "header") |>
    flextable::merge_v(part = "header")  |>
    flextable::align(align = "center", part = "body") |>
    flextable::bg(bg = colour.head.fill, part = "header") |>
    flextable::bg(i=grep(nm.total,dates[[col.total]]),bg = colour.total.fill) |>
    flextable::style(part = "header"
                     ,pr_t = flextable::fp_text_default( # стиль шапки
                       font.size =font.size.header
                       ,color=colour.head.text
                       ,bold = bold.head)) |>
    flextable::style(part = "body",pr_t = flextable::fp_text_default( # стиль тела
      font.size =font.size.body ,bold = bold.body)) |>
    flextable::style(i=grep(nm.total,dates[[col.total]])
                     ,pr_t = flextable::fp_text_default( font.size =font.size.total
                                                         ,bold = bold.total)) |>
    flextable::colformat_num(big.mark = " "
                             , decimal.mark = ",",na_str = " ") |>
    flextable::colformat_char(na_str=" ") |>
    flextable::align_text_col(align = "left") |>
    flextable::align(align = "center", part = "header") |>
    flextable::font(fontname=font.nm,part = "all") |>
    flextable::autofit()
  if (!is.null(col.lv1)) {
    ft <- flextable::bg(ft,bg = col.lv1,i=1, part = "header")
    ft <- flextable::align(ft,align = lv1.align,i=1, part = "head")
  }
  if (!is.null(col.style.prc)) {
    ft <- flextable::colformat_num(ft,suffix = "%",j = col.style.prc,part="body")
  }
  if (!is.null(col.threshold)) {
    ft <- flextable::bg(ft,j=col.threshold, bg = my_color_bg)
    ft <- flextable::color(ft,j=col.threshold, color=my_color_txt)
  }
  ft
}
