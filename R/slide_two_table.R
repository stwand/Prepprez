#' Add two tables to a slide
#'
#' @param deck - presentation
#' @param tab1 - flexible table
#' @param tab2 - flexible table
#' @param sld_loy - slide layout
#' @param name_slide - slide title
#' @param name_master - master
#'
#' @return Presentation with added slide
#'
#' @export
#'

slide_two_table <- function(deck
                        ,tab1
                        ,tab2
                        ,sld_loy
                        ,name_slide
                        ,name_master) {
  # Добавляем данные слайда
  deck <- deck |>
    officer::add_slide(layout = sld_loy, master = name_master) |>
    officer::ph_with(value = name_slide, location = officer::ph_location_type(type = "title")) |>
    officer::ph_with(value = tab1, location = officer::ph_location_label(ph_label = "tab1")) |>
    officer::ph_with(value = tab2, location = officer::ph_location_label(ph_label = "tab2"))
  deck
}
