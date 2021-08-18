#' A geom that writes points but is called punto
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
#'  geom_point()
#'
#' # the function
#' a_geom_punto
#'
#' # using the function
#' ggplot(data = cars) +
#'  aes(x = speed, y = dist) +
#'  a_geom_point()
#'
a_geom_punto <- function(...){

  ggplot2::geom_point(...)

}
