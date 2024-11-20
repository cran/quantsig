#' Sigmoidal quantile function estimator
#'
#' This function implements the sigmoidal quantile function estimator, which is
#' a smooth nonparametric quantile function estimator based on a newly defined
#' generalized expectile function.
#'
#' @param x numeric whose sample quantiles are wanted.
#' @param p The probability with values in (0,1).
#'
#' @return The estimated quantile.
#'
#' @references Hutson AD. The generalized sigmoidal quantile function.
#' Communications in Statistics-Simulation and Computation. 2024 Feb 1;53(2):799-813.
#'
#' @examples
#'
#' x <- c(1, 2, 3, 4, 5, 6, 7, 8, 8, 9)
#' quantsig(x, 0.5)
#'
#' @export

quantsig <- function(x, p) {

  n <- length(x)

  # Compute data-driven smoothing parameter tau
  tau <- sd(x) / sqrt(n)

  # Initial guess for optimization
  thetastart <- qnorm(p, mean = mean(x), sd = sd(x))

  # Optimization
  opt_result <- optim(par = thetastart, fn = qmin, p=p, x=x, tau=tau, method = "BFGS")
  qhat <- opt_result$par
  return(qhat)
}




