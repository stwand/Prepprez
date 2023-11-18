#' Sets options for a flex table
#'
#' @param theme - Theme (Look flextable)
#' @param font - font
#' @param mark - mark
#' @param pd - padding
#'
#' @return - set
#' @export
#'
set_tab_pres <- function(theme=flextable::theme_vanilla
                         ,font="Century Schoolbook"
                         ,mark = " "
                         ,pd=1
                         ) {
flextable::set_flextable_defaults(theme_fun = theme
                                  ,font.family = font
                                  ,big.mark = mark
                                  ,padding = pd)
}
