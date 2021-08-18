#' Percentage labels the are filled according to proportion where total in x is denominator
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
#'
#' chull(cars$speed, cars$dist) %>%
#' cars[.,] ->
#' cars_c_hull_rows
#'
#' ggplot(data = cars) +
#'   aes(x = speed) +
#'   aes(y = dist) +
#'   geom_point() +
#'   geom_polygon(data = cars_c_hull_rows,
#'   alpha = .5
#'   )
#'
#' ggplot(data = cars) +
#'   aes(x = speed) +
#'   aes(y = dist) +
#'   geom_point() +
#'   geom_polygon(data = cars_c_hull_rows,
#'   alpha = .5
#'   ) +
#'   aes(color = speed >= 10)
geom_c_hull <- function(mapping = NULL, data = NULL,
                                          position = "identity", na.rm = FALSE, show.legend = NA,
                                          inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatChull, geom = ggplot2::GeomPolygon, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


StatChull <- ggplot2::ggproto("StatPropovertimetext",
                                         ggplot2::Stat,
                                         compute_group = function(data, scales) {

                                           chull(data$x, data$y) %>%
                                             data[.,]

                                         },

                                         required_aes = c("x", "y")
)

