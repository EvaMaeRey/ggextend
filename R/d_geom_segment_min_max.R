#' A geom that draws a segment from the minimum x and y to maximum x and y
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
#'  summarize(
#'    min_speed = min(speed),
#'    max_speed = max(speed),
#'    min_dist = min(dist),
#'    max_dist = max(dist)
#'  ) ->
#'  mins_maxs
#'
#' library(ggplot2)
#' ggplot(data = cars) +
#'  aes(x = speed, y = dist) +
#'  geom_point() +
#'  geom_segment(data = mins_maxs,
#'      aes(x = min_speed, xend = max_speed,
#'          y = min_dist, yend = max_dist))
#'  layer_data(last_plot(), i = 2)
#'
#' # the proto
#' "StatSegmentminmax"
#' d_geom_segment_mins_maxs
#'
#' # using the function
#' ggplot(data = cars) +
#'  aes(x = speed, y = dist) +
#'  geom_point() +
#'  d_geom_segment_mins_maxs()
#'  layer_data(last_plot(), i = 2)
#'
d_geom_segment_mins_maxs <- function(mapping = NULL,
                              data = NULL,
                              position = "identity",
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE,
                              ...) {
  ggplot2::layer(
    stat = StatSegmentminmax,
    geom = ggplot2::GeomSegment,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

StatSegmentminmax <- ggplot2::ggproto(
  "StatSegmentminmax",
  ggplot2::Stat,

  compute_group = function(data, scales) {

    new_data <- data.frame(x = min(data$x))
    new_data$y = min(data$y)
    new_data$xend = max(data$x)
    new_data$yend = max(data$y)

    new_data

  },

  required_aes = c("x", "y")
)
