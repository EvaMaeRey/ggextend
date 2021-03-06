% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/b_geom_ring.R
\name{b_geom_ring}
\alias{b_geom_ring}
\title{A geom that writes points as a ring}
\usage{
b_geom_ring(...)
}
\arguments{
\item{...}{all the geom_point arguments}
}
\value{

}
\description{
A geom that writes points as a ring
}
\examples{
# without the function
library(ggplot2)
ggplot(data = cars) +
 aes(x = speed, y = dist) +
 geom_point(shape = 21,
            size = 3)

# the function
b_geom_ring
b_geom_ring

# using the function
ggplot(data = cars) +
 aes(x = speed, y = dist) +
 b_geom_ring(size = 3) +
 b_geom_ring(aes(fill = speed > 9),
             color = "white",
             size = 8)

# using the function
ggplot(data = cars) +
 aes(x = speed, y = dist) +
 b_geom_ring(fill = "slateblue",
             color = "white",
             size = 8)

}
