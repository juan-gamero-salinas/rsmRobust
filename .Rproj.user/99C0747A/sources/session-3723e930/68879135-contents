#' Plot 2D Robustness of Bootstrap Stationary Points
#'
#' Creates a two-dimensional robustness plot using bootstrap stationary points
#' obtained from \code{bootstrap_stationary_point()}. The function visualizes:
#' \itemize{
#'   \item Bootstrap sample cloud (black, semi-transparent)
#'   \item Estimated stationary point (red triangle)
#'   \item 95\% confidence interval boundaries (blue dashed lines)
#' }
#'
#' This visualization is useful for assessing the robustness and uncertainty
#' in the estimated RSM optimum.
#'
#' @param boot_df A data frame with bootstrap samples in real units (output of
#'   \code{bootstrap_stationary_point()$boot}).
#' @param stationary A named numeric vector giving the stationary point (coded or real),
#'   whose names must match the column names of \code{boot_df}.
#' @param xvar Character string: name of the variable to plot on the x-axis.
#' @param yvar Character string: name of the variable to plot on the y-axis.
#' @param ci List of confidence intervals (output of \code{bootstrap_stationary_point()$ci}).
#' @param xlim Optional numeric vector length 2 for x-axis limits.
#' @param ylim Optional numeric vector length 2 for y-axis limits.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' library(rsmRobust)
#' library(rsm)
#'
#' # Build CCD (central composite design)
#' dsg <- ccd(2, n0 = c(2,2), alpha = "rotatable", randomize = FALSE)
#'
#' # Simulate a quadratic response surface
#' set.seed(1)
#' true_fun <- function(x1, x2){
#'   5 + 0.5*x1 - 0.3*x2 - 0.4*x1^2 - 0.6*x2^2 + 0.2*x1*x2
#' }
#'
#' dsg$y <- with(dsg, true_fun(x1, x2) + rnorm(nrow(dsg), sd = 0.1))
#'
#' # Fit RSM model
#' model <- rsm(y ~ SO(x1, x2), data = dsg)
#'
#' # Bootstrap stationary point
#' boot <- bootstrap_stationary_point(model, B = 50)
#'
#' # Correct stationary point (coded units)
#' stationary_point <- rsm::xs(model)
#'
#' # Make bootstrap column names match stationary point names
#' colnames(boot$boot) <- names(stationary_point)
#'
#' # Produce the robustness plot
#' p <- plot_robustness_2d(
#'   boot_df   = boot$boot,
#'   stationary = stationary_point,
#'   xvar = names(stationary_point)[1],
#'   yvar = names(stationary_point)[2],
#'   ci   = boot$ci
#' )
#'
#' print(p)
#' }
#' @import ggplot2
#' @import stats
#' @export
plot_robustness_2d <- function(boot_df,
                               stationary,
                               xvar,
                               yvar,
                               ci,
                               xlim = NULL,
                               ylim = NULL) {

  # --- Optional: auto-harmonize names ---------------------------------------
  normalize_names <- function(nm) sub("\\.as\\.is$", "", nm)

  if (!all(c(xvar, yvar) %in% names(boot_df))) {
    names(boot_df) <- normalize_names(names(boot_df))
  }
  if (!all(c(xvar, yvar) %in% names(ci))) {
    names(ci) <- normalize_names(names(ci))
  }
  if (is.null(names(stationary)) || any(names(stationary) == "")) {
    if (length(stationary) >= 2) {
      names(stationary)[seq_along(stationary)] <- names(boot_df)[seq_along(stationary)]
    }
  }
  # --------------------------------------------------------------------------

  # Extract CI bounds
  x_ci <- ci[[xvar]]
  y_ci <- ci[[yvar]]

  p <- ggplot(boot_df, aes(x = .data[[xvar]], y = .data[[yvar]])) +
    geom_point(alpha = 0.05, color = "black", size = 2) +
    geom_point(
      data = data.frame(x = stationary[xvar], y = stationary[yvar]),
      aes(x = .data[["x"]], y = .data[["y"]]),
      color = "red", shape = 17, size = 3
    ) +
    geom_vline(xintercept = x_ci, linetype = "dashed", color = "blue") +
    geom_hline(yintercept = y_ci, linetype = "dashed", color = "blue") +
    theme_minimal() +
    labs(
      x = xvar,
      y = yvar,
      title = "2D Robustness Plot for Bootstrap Stationary Point"
    ) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white"),
      axis.ticks = element_line(color = "black"),
      axis.text.x = element_text(size = 11),
      axis.text.y = element_text(size = 11)
    )

  if (!is.null(xlim)) p <- p + coord_cartesian(xlim = xlim)
  if (!is.null(ylim)) p <- p + coord_cartesian(ylim = ylim)

  p
}
