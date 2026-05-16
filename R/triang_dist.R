.check_params <- function(min, max, mode) {
  if (min >= max) {
    stop("min must be smaller than max")
  }

  if (mode < min || mode > max) {
    stop("mode must be between min and max")
  }
}

.dtriang_one <- function(x, min, max, mode) {
  .check_params(min, max, mode)

  if (x < min || x > max) {
    return(0)
  }

  if (mode == min) {
    return(2 * (max - x) / (max - min)^2)
  }

  if (mode == max) {
    return(2 * (x - min) / (max - min)^2)
  }

  if (x <= mode) {
    2 * (x - min) / ((max - min) * (mode - min))
  } else {
    2 * (max - x) / ((max - min) * (max - mode))
  }
}

#' Density of the triangular distribution
#'
#' Computes the density of a triangular distribution.
#'
#' @param x Numeric vector of quantiles.
#' @param min Lower limit of the distribution.
#' @param max Upper limit of the distribution.
#' @param mode Mode of the distribution.
#'
#' @return A numeric vector with the density values.
#' @export
#'
#' @examples
#' dtriang(c(0, 0.5, 1), min = 0, max = 1, mode = 0.5)
dtriang <- Vectorize(
  .dtriang_one,
  vectorize.args = c("x", "min", "max", "mode"),
  USE.NAMES = FALSE
)

.ptriang_one <- function(q, min, max, mode) {
  .check_params(min, max, mode)

  if (q <= min) {
    return(0)
  }

  if (q >= max) {
    return(1)
  }

  if (mode == min) {
    return(1 - ((max - q) / (max - min))^2)
  }

  if (mode == max) {
    return(((q - min) / (max - min))^2)
  }

  if (q <= mode) {
    (q - min)^2 / ((max - min) * (mode - min))
  } else {
    1 - (max - q)^2 / ((max - min) * (max - mode))
  }
}

#' Distribution function of the triangular distribution
#'
#' Computes the cumulative probability of a triangular distribution.
#'
#' @param q Numeric vector of quantiles.
#' @param min Lower limit of the distribution.
#' @param max Upper limit of the distribution.
#' @param mode Mode of the distribution.
#'
#' @return A numeric vector with cumulative probabilities.
#' @export
#'
#' @examples
#' ptriang(c(0, 0.5, 1), min = 0, max = 1, mode = 0.5)
ptriang <- Vectorize(
  .ptriang_one,
  vectorize.args = c("q", "min", "max", "mode"),
  USE.NAMES = FALSE
)

.qtriang_one <- function(p, min, max, mode) {
  .check_params(min, max, mode)

  if (p < 0 || p > 1) {
    stop("p must be between 0 and 1")
  }

  mode_prob <- (mode - min) / (max - min)

  if (p < mode_prob) {
    min + sqrt(p * (max - min) * (mode - min))
  } else {
    max - sqrt((1 - p) * (max - min) * (max - mode))
  }
}

#' Quantile function of the triangular distribution
#'
#' Computes the inverse cumulative probability of a triangular distribution.
#'
#' @param p Numeric vector of probabilities between 0 and 1.
#' @param min Lower limit of the distribution.
#' @param max Upper limit of the distribution.
#' @param mode Mode of the distribution.
#'
#' @return A numeric vector with quantiles.
#' @export
#'
#' @examples
#' qtriang(c(0.25, 0.5, 0.75), min = 0, max = 1, mode = 0.5)
qtriang <- Vectorize(
  .qtriang_one,
  vectorize.args = c("p", "min", "max", "mode"),
  USE.NAMES = FALSE
)

#' Random generation for the triangular distribution
#'
#' Generates random values from a triangular distribution.
#'
#' @param n Number of observations.
#' @param min Lower limit of the distribution.
#' @param max Upper limit of the distribution.
#' @param mode Mode of the distribution.
#'
#' @return A numeric vector with random values.
#' @export
#'
#' @examples
#' set.seed(1)
#' rtriang(5, min = 0, max = 1, mode = 0.5)
rtriang <- function(n, min, max, mode) {
  if (length(n) > 1) {
    n <- length(n)
  }

  n <- as.integer(n[1])
  if (is.na(n) || n < 0) {
    stop("n must be a non-negative integer")
  }

  if (n == 0) {
    return(numeric())
  }

  u <- stats::runif(n)
  qtriang(u, min, max, mode)
}
