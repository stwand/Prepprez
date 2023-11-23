#' Add tables and a graph
#'
#' @param deck - presentation
#' @param tab1 - flexible table
#' @param gr1 - Graph in format ggplot2
#' @param sld_loy - slide layout
#' @param name_slide - slide title
#' @param name_master - master
#'
#' @return Presentation with added slide
#' @export
#'
slide_table_graph <- function(deck
                        ,tab1
                        ,gr1
                        ,sld_loy
                        ,name_slide
                        ,name_master) {
  # Добавляем данные слайда
  deck <- deck |>
    officer::add_slide(layout = sld_loy, master = name_master) |>
    officer::ph_with(value = name_slide, location = officer::ph_location_type(type = "title")) |>
    officer::ph_with(value = tab1, location = officer::ph_location_label(ph_label = "tab1")) |>
    officer::ph_with(rvg::dml(ggobj=gr1,bg = "transparent"), location = officer::ph_location_label(ph_label = "gr1"))
  deck
}
