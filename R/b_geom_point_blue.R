#' A geom that writes points and default color is blue
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
#'  geom_point(color = "blue")
#'
#' # the function
#' b_geom_point_blue
#' b_geom_point_blue
#'
#' # using the function
#' ggplot(data = cars) +
#'  aes(x = speed, y = dist) +
#'  b_geom_point_blue()
#'
b_geom_point_blue <- function(...){

  ggplot2::geom_point(color = "blue", ...)

}
