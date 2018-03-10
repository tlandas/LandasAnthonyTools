
#' Log Likelihood
#'
#' @param x is data
#' @param  fun is a distribution function
#' @param  int is the interval of x values
#'
#' Calculates the log likelihood of a datset over a certain distribution and interval determined by the user
#' @export
#' @examples
#' data(d)
#' binomial = function(theta, x) {dbinom(x, 20, prob = 1 / (1 + exp(- theta)), log = TRUE)}
#' logL(d, binomial, c(-100, 100))

logL <-function(x, fun, int)
{
  LL <- function(theta)#Theta Est
  {
   sum(fun(theta, x))
  }

  MLE <- optimize(LL, maximum = TRUE, int)#Likelihood Est
  return(MLE$maximum)
}

#' GGPlot Wrapper
#'
#' @param data is data
#' @param xcol is the number of columns in x
#' @param ycol is the number of columns in y
#' @param colorVar is the color of the data points in the plot
#'
#' Creates a graph
#' @export
#' @examples
#' data(flights)
#' ggplotW(flights, "dep_time", "dep_delay", color = blue)
ggplotW <- function(data, xcol, ycol, colorVar)
{
  plot <- ggplot(data=data, aes(x = xcol, y = ycol, color = colorVar)) + geom_point()
  return(plot)
}

#' Matrix Adjuster
#'
#' @param data is data
#' @param colName is the column the user wants, in quotes
#'
#' Uses dpylr on a matrix
#' @export
#' @examples
#' data(flights)
#' adjMat(flights, "dep_time")
adjMat <- function(data, colName)
{
  column = dpylr::select(data, dpylr::contains(colName))
  return(column)
}

#' Measures of Spread
#'
#' @param x is a vector
#'
#' Gives the mean, variance and standard deviation of a population from data, x.
#' @export
#' @examples
#' data(d)
#' spread(d)
spread <- function(d)
{
  moo = sum(d$x* d$p)

  sig2 = (sum(d$x - moo)^2) * d$p

  sD = sqrt(sig2)
}

#' Measures of Spread w/ Checks
#'
#' @param x is a vector
#'
#' Gives the mean, variance and standard deviation of a population from data, x in the form of a list. Has checks implemented to ensure the data given is proper
#' @export
#' @examples
#' data(d)
#' spread(d)
spreadCheck <- function(d)
{
  stopifnot(length(d$x) != 0)#Value Checks for x
  stopifnot(is.finite(d$x))
  stopifnot(is.numeric(d$x))
  stopifnot(length(d$p) != 0)#p
  stopifnot(is.finite(d$p))
  stopifnot(is.numeric(d$p))
  stopifnot(all.equal(1, sum(d$p)))

  moo = sum(d$x * d$p)

  sig2 = sum(((d$x - moo)^2) * d$p)

  sD = (sqrt(sig2))

  list(mu = moo, variance = sig2, StandarDev = sD)
}

#' MyApply
#'
#' @param x is matrix
#' @param MARGIN is the dimiensions of the matrix.
#' @param FUN is a predefined function.
#'
#' Returns a simplified array similar to other apply functiion already in R.
#' @export
#' @examples
#' data(d)
#' myapply(d, 1, mean)
myapply <- function(X, MARGIN, FUN, ...)
{
  R = dim(X)[1]
  C = dim(X)[2]
  f = match.fun(FUN)

  if (MARGIN == 1)
  {
    result = list()
    for (i in 1:R)
    {
      result[[i]] = f(X[i,], ...)
    }
  }
  else if(MARGIN == 2)
  {
    result = list()
    for (j in 1:C)
    {
      result [[j]] = f(X[, j], ...)
    }
  }
  return(simplify2array(result))
}
