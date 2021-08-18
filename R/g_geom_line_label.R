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
#'   mutate(speed_cat = ifelse(speed > 8,
#'                                'Fast', 'Slow')) ->
#' my_cars
#'
#' my_cars %>%
#'   group_by(speed_cat) %>%
#'   filter(speed == max(speed)) ->
#'  cars_group_endpoint
#'
#' library(ggplot2)
#' ggplot(data = my_cars) +
#'  aes(x = speed, y = dist) +
#'  aes(color = speed > 8) +
#'  geom_line() +
#'  geom_text(data = cars_group_endpoint,
#'            aes(label = speed_cat),
#'            hjust = 0, nudge_x = .2)
#' layer_data(last_plot(), 2)
#'
#' # the proto
#' StatLineendlabel
#'
#' # the function
#' g_geom_line_label
#'
#' # using the function
#' ggplot(data = my_cars) +
#'  aes(x = speed, y = dist) +
#'  geom_line() +
#'  g_geom_line_label() +
#'  aes(label = speed_cat)
#'  layer_data(last_plot(), 2)
#'
g_geom_line_label <- function(mapping = NULL,
                              data = NULL,
                              position = "identity",
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE,
                              ...) {
  ggplot2::layer(
    stat = StatLineendlabel,
    geom = ggplot2::GeomText,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

StatLineendlabel <- ggplot2::ggproto(
  "StatLineendlabel",
  ggplot2::Stat,

  compute_group = function(data, scales) {

    data %>%
      group_by(label) %>%
      filter(x == max(x)) %>%
      ungroup()

    },

  required_aes = c("x", "y", "label")
)

