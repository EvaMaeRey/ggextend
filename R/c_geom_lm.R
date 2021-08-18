#' A geom that draws the simple linear ols model
#'
#' @param ... all the geom_point arguments
#'
#' @return
#' @export
#'
#' @examples
#' # without the function
#' library(ggplot2)
#' ggplot(data = cars) +
#'  aes(x = speed, y = dist) +
#'  geom_point() +
#'  geom_smooth(method = lm, se = FALSE)
#'
#' # the function
#' c_geom_lm
#'
#' # using the function
#' ggplot(data = cars) +
#'  aes(x = speed, y = dist) +
#'  geom_point() +
#'  c_geom_lm()
#'
c_geom_lm <- function(...){

  ggplot2::geom_smooth(method = lm, se = FALSE, ...)

}
