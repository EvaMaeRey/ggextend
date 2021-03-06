% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/r_geom_lm_sample.R
\name{r_geom_lm_sample}
\alias{r_geom_lm_sample}
\title{A geom that shows a sampling of points}
\usage{
r_geom_lm_sample(
  mapping = NULL,
  data = NULL,
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
)
}
\arguments{
\item{...}{all the geom_point arguments}
}
\value{

}
\description{
A geom that shows a sampling of points
}
\examples{
# without the function
library(magrittr)
library(dplyr)
set.seed(1214)
cars \%>\%
 slice_sample( n = 10) ->
 cars_sample


library(ggplot2)
ggplot(data = cars) +
 aes(x = speed, y = dist) +
 geom_point() +
 geom_point(data = cars_sample,
            color = "magenta") +
 geom_smooth(method = lm, se = F,
             data = cars_sample)

# the proto and function
"StatSamplepoint"
r_geom_lm_sample

# using the function

my_seed <- sample(1:100, 1)
ggplot(data = cars) +
 aes(x = speed, y = dist) +
 geom_point() +
 geom_smooth(method = lm, se = F) +
 q_geom_point_sample(color = "red", seed = my_seed) +
 r_geom_lm_sample(color = "red", seed = my_seed)

}
