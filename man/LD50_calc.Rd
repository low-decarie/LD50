% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LD50_calc.R
\name{LD50_calc}
\alias{LD50_calc}
\title{Calculate concentrations of stressor that decrease growth rate by half (LD50)}
\usage{
LD50_calc(growth, stressor)
}
\arguments{
\item{growth}{growth rate}

\item{stressor}{stressor concentration}
}
\value{
data.frame containing values for upper and lower LD50
}
\description{
Calculate concentrations of stressor that decrease growth rate by half (LD50)
}
\examples{
simulated_data <- x <- 1:100
a <- 10 ; b <- 50 ; c <- 0.1
gaussian_data <- data.frame(x=x,
                           y=a*exp(-((x-b)^2)/2*c^2)+rnorm(length(x)))

growth <- gaussian_data$y
stressor <- x



}
