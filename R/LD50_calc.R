#' Calculate concentrations of stressor that decrease growth rate by half (LD50)
#'
#' @param growth growth rate
#' @param stressor stressor concentration
#'
#' @return data.frame containing values for upper and lower LD50
#' @export
#'
#' @examples
#' simulated_data <- x <- 1:100
#'a <- 10 ; b <- 50 ; c <- 0.1
#'gaussian_data <- data.frame(x=x,
#'                            y=a*exp(-((x-b)^2)/2*c^2)+rnorm(length(x)))
#'
#'growth <- gaussian_data$y
#'stressor <- x
#'
#'
#'
LD50_calc <- function(growth, stressor){

#Move so that y=0 at LD50s
growth <- growth-0.5*max(growth)

#Apply a smoother
fit <- loess(growth~stressor)

#Extract roots
roots <- rootSolve::uniroot.all(f = function(x)predict(fit,newdata = x), interval = c(min(stressor),
                                                  max(stressor)))

# ID lower and upper roots
return(data.frame(lower=min(roots),
                  upper=max(roots)))

}
