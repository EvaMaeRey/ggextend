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
#'  lm(dist ~ speed, data = .) ->
#'  cars_model
#'
#'  tibble(y_val = cars_model$coefficients[1],
#'  x_val = 0) ->
#'  cars_model_intercept
#'
#' library(ggplot2)
#' ggplot(data = cars) +
#'  aes(x = speed, y = dist) +
#'  geom_point() +
#'  geom_smooth(method = lm) +
#'  geom_point(data = cars_model_intercept,
#'      aes(x = x_val,
#'          y = y_val),
#'          color = "red",
#'          size = 3)
#'
#' # the proto
#' StatMinsMaxs
#'
#' # the function
#' e_geom_lm_intercept
#'
#' # using the function
#' ggplot(data = cars) +
#'  aes(x = speed, y = dist) +
#'  geom_point() +
#'  geom_smooth(method = lm) +
#'  e_geom_lm_intercept(size = 4) +
#'  aes(color = speed > 12)
#'
e_geom_lm_intercept <- function(mapping = NULL,
                              data = NULL,
                              position = "identity",
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatOlsintercept,
    geom = ggplot2::GeomPoint,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

StatOlsintercept <- ggplot2::ggproto(
  "StatOlsintercept",
  ggplot2::Stat,

  compute_group = function(data, scales) {

    model <- lm(data$y ~ data$x)

    data.frame(y = model$coefficients[1],
               x = 0)

    },

  required_aes = c("x", "y")
)

