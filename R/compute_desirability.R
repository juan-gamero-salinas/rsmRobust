#' Compute Overall Desirability for Two Responses
#'
#' This function computes overall desirability based on IOH and UDI responses,
#' using Derringer-Suich desirability functions. IOH is minimized while UDI is
#' maximized. The resulting desirability scores can be used as a single-objective
#' function for multiresponse optimization.
#'
#' @param df A data frame containing columns \code{IOH} and \code{UDI}.
#' @param ioh_range Numeric vector of length 2 giving the min and max
#' acceptable values for IOH (default c(0, 0.2)).
#' @param udi_range Numeric vector of length 2 giving the min and max
#' acceptable values for UDI (default c(35, 100)).
#'
#' @return A numeric vector of overall desirability values.
#'
#' @examples
#' \dontrun{
#' df_demo <- data.frame(IOH = c(0.1, 0.18, 0.05), UDI = c(60, 40, 80))
#' compute_desirability(df_demo)
#' }
#'
#' @import desirability
#' @importFrom stats predict
#' @export
compute_desirability <- function(df,
                                 ioh_range = c(0, 0.2),
                                 udi_range = c(35, 100)) {

  # build desirability functions: IOH minimized, UDI maximized
  iohD <- desirability::dMin(ioh_range[1], ioh_range[2], scale = 1)
  udiD <- desirability::dMax(udi_range[1], udi_range[2], scale = 1)

  overall <- desirability::dOverall(iohD, udiD)

  # names must match the desirability inputs used in dOverall()
  preds <- data.frame(ioh = df$IOH, udi = df$UDI)

  # IMPORTANT: call the generic 'predict()' so S3 dispatch uses predict.desirability
  out <- predict(overall, preds)

  # return a numeric vector (overall desirability)
  as.numeric(out)
}
