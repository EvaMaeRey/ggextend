#' Tiles the are filled according to proportion where total in x is denominator
#'
#' @param mapping
#' @param data
#' @param position
#' @param na.rm
#' @param show.legend
#' @param inherit.aes
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' # do it without function
#' library(tidyverse)
#' gapminder::gapminder %>%
#'   mutate(gdp = gdpPercap * pop) ->
#' my_gapminder
#'
#' my_gapminder %>%
#'   group_by(year, continent) %>%
#'   summarise(gdp = sum(gdp)) %>%
#'   mutate(prop_gdp = gdp/sum(gdp)) %>%
#'   ggplot() +
#'   aes(x = year) +
#'   aes(y = continent) +
#'   geom_tile() +
#'   aes(fill = prop_gdp) +
#'   scale_fill_viridis_c()
#'
#' # the function and proto
#' "StatPropovertime"
#' "StatPropovertimetext"
#'
#' # use function
#' library(ggplot2)
#' library(magrittr)
#' my_gapminder %>%
#' ggplot() +
#'   aes(x = year) +
#'   aes(y = continent) +
#'   h_geom_tile_prop_over_time() +
#'   aes(fill = gdp) +
#'   scale_fill_viridis_c()
h_geom_tile_prop_over_time <- function(mapping = NULL, data = NULL,
                                     position = "identity", na.rm = FALSE, show.legend = NA,
                                     inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatPropovertime, geom = ggplot2::GeomTile, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


StatPropovertime <- ggplot2::ggproto("StatPropovertime",
                                     ggplot2::Stat,
                                     compute_panel = function(data, scales) {

                                       data %>%
                                         dplyr::group_by(x, y) %>%
                                         dplyr::summarise(fill = sum(fill)) %>%
                                         dplyr::mutate(fill = fill/sum(fill)) %>%
                                         dplyr::ungroup()

                                     },

                                     required_aes = c("x", "y", "fill")
)



StatPropovertimetext <- ggplot2::ggproto("StatPropovertimetext",
                                         ggplot2::Stat,
                                         compute_panel = function(data, scales) {

                                           data %>%
                                             dplyr::group_by(x, y) %>%
                                             dplyr::summarise(fill = sum(fill)) %>%
                                             dplyr::mutate(fill = fill/sum(fill)) %>%
                                             dplyr::ungroup() %>%
                                             dplyr::mutate(label = paste0(100*fill %>% round(2), "%"))

                                         },

                                         required_aes = c("x", "y", "fill", "label")
)

