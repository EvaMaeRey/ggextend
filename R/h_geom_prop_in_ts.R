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
#' library(ggplot2)
#' library(magrittr)
#' my_gapminder %>%
#' ggplot() +
#'   aes(x = year) +
#'   aes(y = continent) +
#'   geom_tile_prop_over_time() +
#'   aes(fill = gdp) +
#'   scale_fill_viridis_c()
geom_tile_prop_over_time <- function(mapping = NULL, data = NULL,
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

#' Title
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
#' library(ggplot2)
#' library(magrittr)
#' gapminder::gapminder %>%
#' dplyr::mutate(gdp = gdpPercap * pop) %>%
#' ggplot() +
#'   aes(x = year) +
#'   aes(y = continent) +
#'   geom_tile_prop_over_time() +
#'   aes(fill = gdp) +
#'   scale_fill_viridis_c() +
#'   aes(label = gdp) +
#'   geom_tile_prop_over_time_text()
#'
#' gapminder::gapminder %>%
#' dplyr::mutate(gdp = gdpPercap * pop) %>%
#' ggplot() +
#'   aes(x = year) +
#'   aes(y = continent) +
#'   geom_tile_prop_over_time() +
#'   aes(fill = gdp) +
#'   scale_fill_viridis_c() +
#'   aes(label = gdp) +
#'   geom_tile_prop_over_time_text() +
#'   facet_wrap(facets = vars(pop > 1000000))
#'
#' gapminder::gapminder %>%
#' ggplot() +
#'   aes(x = year) +
#'   aes(y = continent) +
#'   aes(fill = 1, label = 1) + # use 1 to count times continent is observed
#'   geom_tile_prop_over_time(color = "oldlace") +
#'   labs(fill = "proportion\nof countries\nin each time\nperiod") +
#'   geom_tile_prop_over_time_text(size = 3) +
#'   facet_wrap(facets = vars(ifelse(gdpPercap > 10000,
#'   "gdp per cap > 10000", "gdp per cap < 10000")),
#'   ncol = 1) +
#'   scale_fill_viridis_c()
#'
geom_tile_prop_over_time_text <- function(mapping = NULL, data = NULL,
                                          position = "identity", na.rm = FALSE, show.legend = NA,
                                          inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatPropovertimetext, geom = ggplot2::GeomText, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

