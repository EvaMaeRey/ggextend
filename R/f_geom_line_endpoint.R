#' A geom that draws a point at the intercept of linear model estimate
#'
#' @param ... all the geom_point arguments
#'
#' @return
#' @export
#'
#' @examples
#' # without the function
#' library(magrittr)
#' library(dplyr)
#' cars %>%
#'   mutate(speed_cat = speed > 8) %>%
#'   group_by(speed_cat) %>%
#'   filter(speed == max(speed)) ->
#'  cars_group_endpoint
#'
#' library(ggplot2)
#' ggplot(data = cars) +
#'  aes(x = speed, y = dist) +
#'  aes(color = speed > 8) +
#'  geom_line() +
#'  geom_point(data = cars_group_endpoint)
#' layer_data(last_plot(), 2)
#'
#' # the proto
#' "StatEndpoint"
#' f_geom_line_endpoint
#'
#' # the function
#' f_geom_line_endpoint
#'
#' # using the function
#' ggplot(data = cars) +
#'  aes(x = speed, y = dist) +
#'  geom_line() +
#'  f_geom_line_endpoint(size = 4) +
#'  aes(color = speed > 9)
#'  layer_data(last_plot(), 2)
#'
f_geom_line_endpoint <- function(mapping = NULL,
                              data = NULL,
                              position = "identity",
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatEndpoint,
    geom = ggplot2::GeomPoint,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

StatEndpoint <- ggplot2::ggproto(
  "StatEndpoint",
  ggplot2::Stat,

  compute_group = function(data, scales) {

    data %>%
      filter(x == max(x))

    },

  required_aes = c("x", "y")
)

