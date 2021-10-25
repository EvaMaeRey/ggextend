GeomXmeantext <- ggplot2::ggproto("GeomXmeantext", ggplot2::Geom,
                                   draw_panel = function(data, panel_params, coord) {

                                     ranges <- coord$backtransform_range(panel_params)


                                    data_raw <- data

                                     data$x <- unique(data_raw$x)
                                     # data$yend <- mean(data$y)


                                     data$count <- NA

                                     for (i in 1:length(data$x)){

                                       data$count[i] <- nrow(data_raw[data_raw$x == data$x[i],])

                                     }

                                     data$y <- data$count #+ (ranges$y[1] + ranges$y[2])*.5/10
                                     # data$y    <- (ranges$y[1] + ranges$y[2])*3/10
                                     # data$xend <- ranges$x[2]
                                     data$label <- data$count

                                     data <- unique(data)

                                     GeomText$draw_panel(unique(data), panel_params, coord)

                                   },

                                   default_aes = ggplot2::aes(colour = "black",
                                                              size = 5,
                                                              angle = 0,
                                                              linetype = 1,
                                                              fill = "white",
                                                              alpha = 1,
                                                              vjust = -.4),
                                   required_aes = "x",

                                   draw_key = ggplot2::draw_key_label
)



#' Lines defined by mean value of y
#'
#'
#' @param mapping
#' @param data
#' @param ...
#' @param x
#' @param na.rm
#' @param show.legend
#'
#' @return
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(data = cars) +
#' aes(x = speed > 15) + geom_bar() +
#' geom_x_mean_text() +
#' geom_x_mean_text(vjust = 1.4) +
#' aes(fill = dist < 15,
#'     group = dist < 15,
#'     color = dist < 15)
#'
#' scale_y_continuous(limits = c(0, 40))
#'
#' ggplot2::layer_data(last_plot(), 2)
geom_x_mean_text <- function(mapping = NULL, data = NULL,
                              ...,
                              x,
                              na.rm = FALSE,
                              show.legend = NA) {

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = ggplot2::StatIdentity,
    geom = GeomXmeantext,
    position = ggplot2::PositionIdentity,
    show.legend = show.legend,
    inherit.aes = TRUE,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

