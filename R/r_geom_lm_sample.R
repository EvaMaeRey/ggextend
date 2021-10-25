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
#'             color = "magenta") +
#'  geom_smooth(method = lm, se = F,
#'              data = cars_sample)
#'
#' # the proto and function
#' "StatSamplepoint"
#' r_geom_lm_sample
#'
#' # using the function
#'
#' my_seed <- sample(1:100, 1)
#' ggplot(data = cars) +
#'  aes(x = speed, y = dist) +
#'  geom_point() +
#'  geom_smooth(method = lm, se = F) +
#'  q_geom_point_sample(color = "red", seed = my_seed) +
#'  r_geom_lm_sample(color = "red", seed = my_seed)
#'
#'
r_geom_lm_sample <- function(mapping = NULL,
                                data = NULL,
                                position = "identity",
                                na.rm = FALSE,
                                show.legend = NA,
                                inherit.aes = TRUE,
                                # sample_size = 10,
                                ...) {


  message("sample size is 10 by default")

  ggplot2::layer(
    stat = StatSamplelm,
    geom = ggplot2::GeomLine,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )

}

StatSamplelm <- ggplot2::ggproto(
  "StatSamplelm",
  ggplot2::Stat,

  compute_group = function(data, scales, sample_size = 10, seed = NULL) {

    if(is.null(seed)){ seed <- sample(3000:4000, 1) }

    set.seed(seed)

    data %>%
      slice_sample(n = sample_size) ->
    data_sample

      lm(y ~ x, data = data_sample) ->
    model

    data.frame(x = data_sample$x,
                 y = model$fitted.values)



  },

  required_aes = c("x", "y")
)

