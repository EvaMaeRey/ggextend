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
#' chull(cars$speed, cars$dist) %>% # index of rim points
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
#'   k_geom_c_hull_hollow(alpha = .5) +
#'   aes(color = speed >= 10)
k_geom_c_hull_hollow <- function(mapping = NULL, data = NULL,
                                          position = "identity", na.rm = FALSE, show.legend = NA,
                                          inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatChull, geom = GeomPolygonHollow, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

GeomPolygonHollow <- ggplot2::ggproto("GeomPolygonHollow",
                              ggplot2::GeomPolygon,
                              default_aes = ggplot2::aes(alpha = NA, fill = NA, linetype = 1, lwd = 1)
)



StatChull <- ggplot2::ggproto("StatChull",
                              ggplot2::Stat,
                              compute_group = function(data, scales) {

                                      chull(data$x, data$y) %>%
                                             data[.,]

                                         },

                                         required_aes = c("x", "y")
)

