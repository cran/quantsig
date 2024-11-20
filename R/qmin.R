qmin <- function(theta, p, x, tau) {
  residuals <- -theta + x
  sum((-1 + 2 * p) * residuals +
        tau * log1p(exp(-residuals / tau)) +
        tau * log1p(exp(residuals / tau)))
}
