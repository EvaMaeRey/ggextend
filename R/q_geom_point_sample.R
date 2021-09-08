#' A geom that shows a sampling of points
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
#' set.seed(1214)
#' cars %>%
#'  slice_sample( n = 10) ->
#'  cars_sample
#'
#'
#' library(ggplot2)
#' ggplot(data = cars) +
#'  aes(x = speed, y = dist) +
#'  geom_point() +
#'  geom_point(data = cars_sample,
#'             color = "magenta")
#'
#' # the proto and function
#' "StatSamplepoint"
#' q_geom_point_sample
#'
#' # using the function
#' ggplot(data = cars) +
#'  aes(x = speed, y = dist) +
#'  geom_point() +
#'  q_geom_point_sample(color = "red") +
#'  q_geom_point_sample(color = "green", sample_size = 5) +
#'  q_geom_point_sample(sample_size = 2, color = "blue")
#'
q_geom_point_sample <- function(mapping = NULL,
                                data = NULL,
                                position = "identity",
                                na.rm = FALSE,
                                show.legend = NA,
                                inherit.aes = TRUE,
                                # sample_size = 10,
                                ...) {
  ggplot2::layer(
    stat = StatSamplepoint,
    geom = ggplot2::GeomPoint,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

StatSamplepoint <- ggplot2::ggproto(
  "StatSamplepoint",
  ggplot2::Stat,

  compute_group = function(data, scales, sample_size = 10, seed = NULL) {

    if(is.null(seed)){ seed <- sample(3000:4000, 1) }

    set.seed(seed)

    data %>%
      slice_sample(n = sample_size)

  },

  required_aes = c("x", "y")
)

