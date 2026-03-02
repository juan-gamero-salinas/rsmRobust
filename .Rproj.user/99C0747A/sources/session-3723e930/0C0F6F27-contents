#' Run Fractional Factorial Screening Design
#'
#' Creates a fractional factorial design using the FrF2 package and
#' optionally merges response data (e.g., IOH, UDI) into the design.
#'
#' @param nfactors Number of factors.
#' @param resolution Desired resolution of the design.
#' @param factor_names Character vector of factor names.
#' @param responses Optional data frame with responses to merge.
#'
#' @return A data frame with the fractional factorial design (and responses if provided).
#'
#' @import FrF2
#' @export
run_fractional_screening <- function(nfactors,
                                     resolution = 5,
                                     factor_names,
                                     responses = NULL) {

  design <- FrF2::FrF2(
    nfactors = nfactors,
    resolution = resolution,
    randomize = FALSE,
    default.levels = c(-1, 1),
    factor.names = factor_names
  )

  design_df <- as.data.frame(design)

  if (!is.null(responses)) {
    design_df <- cbind(design_df, responses)
  }

  return(design_df)
}
