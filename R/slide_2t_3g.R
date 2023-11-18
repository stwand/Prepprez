#' Add two tables and a graph
#'
#' @param deck - presentation
#' @param tab1 - flexible table
#' @param tab2 - flexible table
#' @param gr1 - Graph in format ggplot2
#' @param gr2 - Graph in format ggplot2
#' @param gr3 - Graph in format ggplot2
#' @param sld_loy - slide layout
#' @param name_slide - slide title
#' @param name_master - master
#'
#' @return Presentation with added slide
#'
#' @export
#'
slide_2t_3g <- function(deck
                        ,tab1
                        ,tab2
                        ,gr1
                        ,gr2
                        ,gr3
                        ,sld_loy
                        ,name_slide
                        ,name_master) {
  # Добавляем данные слайда
  deck <- deck |>
    officer::add_slide(layout = sld_loy, master = name_master) |>
    officer::ph_with(value = name_slide, location = officer::ph_location_type(type = "title")) |>
    officer::ph_with(value = tab1, location = officer::ph_location_label(ph_label = "tab1")) |>
    officer::ph_with(value = tab2, location = officer::ph_location_label(ph_label = "tab2")) |>
    officer::ph_with(rvg::dml(ggobj=gr1,bg = "transparent"), location = officer::ph_location_label(ph_label = "gr1")) |>
    officer::ph_with(rvg::dml(ggobj=gr2,bg = "transparent"), location = officer::ph_location_label(ph_label = "gr2")) |>
    officer::ph_with(rvg::dml(ggobj=gr3,bg = "transparent"), location = officer::ph_location_label(ph_label = "gr3"))
  deck
}
