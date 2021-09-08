#' Title
#'
#' @param fill
#' @param color
#'
#' @return
#' @export
#'
#' @examples
#' ggplot(cars) +
#'   aes(x = speed) +
#'   aes(y = dist) +
#'   geom_point(color = "plum4") +
#'   theme_void() +
#'   theme(plot.background = ggplot2::element_rect(fill = "plum1", color = "plum1"),
#'         panel.background = ggplot2::element_rect(fill = "plum1", color = "plum1"))
#'
#' ggplot(cars) +
#'   aes(x = speed) +
#'   aes(y = dist) +
#'   geom_point(color = "plum4") +
#'   theme_void_fill(fill = "plum1")
theme_void_fill <- function(fill = "linen", color = fill){

  ggplot2::theme_void() %+replace%
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = fill, color = color),
                   panel.background = ggplot2::element_rect(fill = fill, color = color))

}
