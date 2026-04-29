#' Recycle vectors using base R recycling rules
#'
#' @keywords internal
recycle_triang_args <- function(...) {
  args <- list(...)
  arg_lengths <- vapply(args, length, integer(1))

  if (any(arg_lengths == 0L)) {
    return(lapply(args, function(arg) arg[FALSE]))
  }

  output_length <- max(arg_lengths)
  bad_lengths <- arg_lengths != 1L & output_length %% arg_lengths != 0L

  if (any(bad_lengths)) {
    warning(
      "longer object length is not a multiple of shorter object length",
      call. = FALSE
    )
  }

  lapply(args, rep_len, length.out = output_length)
}

#' Validate triangular distribution parameters
#'
#' @keywords internal
validate_triang_params <- function(min, max, mode) {
  invalid_bounds <- !is.na(min) & !is.na(max) & min >= max

  if (any(invalid_bounds)) {
    stop("`min` must be strictly smaller than `max`.", call. = FALSE)
  }

  invalid_mode <- !is.na(min) & !is.na(max) & !is.na(mode) &
    (mode < min | mode > max)

  if (any(invalid_mode)) {
    stop("`mode` must be in the interval [`min`, `max`].", call. = FALSE)
  }

  invisible(NULL)
}

#' Density of the triangular distribution
#'
#' Computes the probability density function of a triangular distribution with
#' lower limit `min`, upper limit `max`, and mode `mode`.
#'
#' @param x Numeric vector of quantiles.
#' @param min Numeric vector of lower limits.
#' @param max Numeric vector of upper limits.
#' @param mode Numeric vector of modes. Each value must lie between `min` and
#'   `max`, inclusive.
#'
#' @return A numeric vector with the density values.
#' @export
#'
#' @examples
#' dtriang(c(0, 0.5, 1), min = 0, max = 1, mode = 0.5)
dtriang <- function(x, min, max, mode) {
  args <- recycle_triang_args(x = x, min = min, max = max, mode = mode)
  x <- args$x
  min <- args$min
  max <- args$max
  mode <- args$mode

  validate_triang_params(min, max, mode)

  density <- rep(NA_real_, length(x))
  missing_values <- is.na(x) | is.na(min) | is.na(max) | is.na(mode)
  outside <- !missing_values & (x < min | x > max)
  inside <- !missing_values & !outside

  density[outside] <- 0

  min_mode <- inside & mode == min
  max_mode <- inside & mode == max
  middle_mode <- inside & mode > min & mode < max

  density[min_mode] <- 2 * (max[min_mode] - x[min_mode]) /
    (max[min_mode] - min[min_mode])^2

  density[max_mode] <- 2 * (x[max_mode] - min[max_mode]) /
    (max[max_mode] - min[max_mode])^2

  left_side <- middle_mode & x <= mode
  right_side <- middle_mode & x > mode

  density[left_side] <- 2 * (x[left_side] - min[left_side]) /
    ((max[left_side] - min[left_side]) * (mode[left_side] - min[left_side]))

  density[right_side] <- 2 * (max[right_side] - x[right_side]) /
    ((max[right_side] - min[right_side]) * (max[right_side] - mode[right_side]))

  density
}

#' Distribution function of the triangular distribution
#'
#' Computes the cumulative distribution function of a triangular distribution
#' with lower limit `min`, upper limit `max`, and mode `mode`.
#'
#' @param q Numeric vector of quantiles.
#' @param min Numeric vector of lower limits.
#' @param max Numeric vector of upper limits.
#' @param mode Numeric vector of modes. Each value must lie between `min` and
#'   `max`, inclusive.
#'
#' @return A numeric vector with probabilities in `[0, 1]`.
#' @export
#'
#' @examples
#' ptriang(c(0, 0.5, 1), min = 0, max = 1, mode = 0.5)
ptriang <- function(q, min, max, mode) {
  args <- recycle_triang_args(q = q, min = min, max = max, mode = mode)
  q <- args$q
  min <- args$min
  max <- args$max
  mode <- args$mode

  validate_triang_params(min, max, mode)

  probability <- rep(NA_real_, length(q))
  missing_values <- is.na(q) | is.na(min) | is.na(max) | is.na(mode)
  below <- !missing_values & q <= min
  above <- !missing_values & q >= max
  inside <- !missing_values & q > min & q < max

  probability[below] <- 0
  probability[above] <- 1

  min_mode <- inside & mode == min
  max_mode <- inside & mode == max
  middle_mode <- inside & mode > min & mode < max

  probability[min_mode] <- 1 -
    ((max[min_mode] - q[min_mode]) / (max[min_mode] - min[min_mode]))^2

  probability[max_mode] <-
    ((q[max_mode] - min[max_mode]) / (max[max_mode] - min[max_mode]))^2

  left_side <- middle_mode & q <= mode
  right_side <- middle_mode & q > mode

  probability[left_side] <- (q[left_side] - min[left_side])^2 /
    ((max[left_side] - min[left_side]) * (mode[left_side] - min[left_side]))

  probability[right_side] <- 1 - (max[right_side] - q[right_side])^2 /
    ((max[right_side] - min[right_side]) *
       (max[right_side] - mode[right_side]))

  probability
}

#' Quantile function of the triangular distribution
#'
#' Computes the inverse cumulative distribution function of a triangular
#' distribution with lower limit `min`, upper limit `max`, and mode `mode`.
#'
#' @param p Numeric vector of probabilities. Values must lie in `[0, 1]`.
#' @param min Numeric vector of lower limits.
#' @param max Numeric vector of upper limits.
#' @param mode Numeric vector of modes. Each value must lie between `min` and
#'   `max`, inclusive.
#'
#' @return A numeric vector of quantiles.
#' @export
#'
#' @examples
#' qtriang(c(0.25, 0.5, 0.75), min = 0, max = 1, mode = 0.5)
qtriang <- function(p, min, max, mode) {
  args <- recycle_triang_args(p = p, min = min, max = max, mode = mode)
  p <- args$p
  min <- args$min
  max <- args$max
  mode <- args$mode

  validate_triang_params(min, max, mode)

  invalid_p <- !is.na(p) & (p < 0 | p > 1)

  if (any(invalid_p)) {
    stop("`p` must contain values in the interval [0, 1].", call. = FALSE)
  }

  quantile <- rep(NA_real_, length(p))
  missing_values <- is.na(p) | is.na(min) | is.na(max) | is.na(mode)
  observed <- !missing_values
  cutoff <- (mode - min) / (max - min)

  lower_tail <- observed & p < cutoff
  upper_tail <- observed & !lower_tail

  quantile[lower_tail] <- min[lower_tail] +
    sqrt(p[lower_tail] * (max[lower_tail] - min[lower_tail]) *
           (mode[lower_tail] - min[lower_tail]))

  quantile[upper_tail] <- max[upper_tail] -
    sqrt((1 - p[upper_tail]) * (max[upper_tail] - min[upper_tail]) *
           (max[upper_tail] - mode[upper_tail]))

  quantile
}

#' Random generation for the triangular distribution
#'
#' Generates random values from a triangular distribution using inverse
#' transform sampling.
#'
#' @param n Number of observations. If `length(n) > 1`, the length is used.
#' @param min Numeric vector of lower limits.
#' @param max Numeric vector of upper limits.
#' @param mode Numeric vector of modes. Each value must lie between `min` and
#'   `max`, inclusive.
#'
#' @return A numeric vector of random values.
#' @export
#'
#' @examples
#' set.seed(1)
#' rtriang(5, min = 0, max = 1, mode = 0.5)
rtriang <- function(n, min, max, mode) {
  n <- if (length(n) > 1L) {
    length(n)
  } else {
    n
  }

  invalid_n <- !is.numeric(n) || length(n) == 0L || is.na(n) ||
    n < 0 || n != floor(n)

  if (invalid_n) {
    stop("`n` must be a non-negative whole number.", call. = FALSE)
  }

  if (n == 0L) {
    return(numeric())
  }

  qtriang(stats::runif(n), min = min, max = max, mode = mode)
}
