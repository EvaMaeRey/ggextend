#' A geom that writes points as a ring
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
#'  geom_point(shape = 21,
#'             size = 3)
#'
#' # the function
#' b_geom_ring
#' b_geom_ring
#'
#' # using the function
#' ggplot(data = cars) +
#'  aes(x = speed, y = dist) +
#'  b_geom_ring(size = 3) +
#'  b_geom_ring(aes(fill = speed > 9),
#'              color = "white",
#'              size = 8)
#'
#' # using the function
#' ggplot(data = cars) +
#'  aes(x = speed, y = dist) +
#'  b_geom_ring(fill = "slateblue",
#'              color = "white",
#'              size = 8)
#'
b_geom_ring <- function(...){

  ggplot2::geom_point(shape = 21, ...)

}
