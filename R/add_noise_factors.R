#' Add Noise to Nuisance Factors
#'
#' Applies normally-distributed noise to one or more factors using
#' a specified transformation function.
#'
#' @param n Number of samples to generate.
#' @param mean Mean of the normal distribution.
#' @param sd Standard deviation of the normal distribution.
#' @param transform_fn A function applied to the normal draw (default is identity).
#'
#' @return A numeric vector of noisy factor values.
#'
#' @export
add_noise_factors <- function(n,
                              mean = 0,
                              sd = 1,
                              transform_fn = function(x) x) {

  noise <- rnorm(n, mean = mean, sd = sd)
  transformed <- transform_fn(noise)

  return(transformed)
}
