#' Sequential RSM Optimization with Desirability
#'
#' Fits a Response Surface model using \pkg{rsm}, extracts the stationary point,
#' and predicts the response at that point. Works with both coded and uncoded
#' data frames. If the data are \code{coded.data}, the function can also return
#' the stationary point in real (uncoded) units.
#'
#' @param data A data frame. If it is a \code{coded.data} object (from \code{rsm::coded.data}),
#'   the stationary point is available in both coded and real units.
#' @param formula A model formula for \code{rsm::rsm()}, e.g. \code{y ~ FO(x1, x2)} or
#'   \code{y ~ SO(x1, x2, x3)}.
#' @param return_real Logical. If \code{TRUE} and \code{data} is \code{coded.data},
#'   also return the stationary point converted to real units (default \code{TRUE}).
#' @param desirability Optional. Either \code{NULL} (default), a function that takes a
#'   numeric vector of predictions and returns a numeric desirability vector, or a
#'   list describing how to build desirability (see Details).
#' @param ... Additional arguments passed to \code{rsm::rsm()}.
#'
#' @details
#' \strong{Desirability integration (optional):}
#' - If \code{desirability} is a function, it will be called on the predicted value(s)
#'   at the stationary point; the result is returned as \code{$desirability}.
#' - If \code{desirability} is a list of the form
#'   \code{list(type = "two-response", ioh_range = c(a,b), udi_range = c(c,d))},
#'   the function will build Derringer–Suich desirabilities using \pkg{desirability}
#'   and compute an overall desirability for a data frame that contains columns
#'   \code{IOH} and \code{UDI}. (This is a convenience wrapper.)
#'
#' @return A list with components:
#' \itemize{
#'   \item \code{model}: the fitted \code{rsm} model.
#'   \item \code{stationary_coded}: stationary point in coded units (named numeric).
#'   \item \code{stationary_real}: stationary point in real units if available (otherwise \code{NULL}).
#'   \item \code{prediction}: model prediction at the stationary point (numeric).
#'   \item \code{desirability}: optional desirability value if \code{desirability} was provided.
#' }
#'
#' @examples
#' \dontrun{
#'
#' library(rsm)
#'
#' # ============================================================
#' # Example 1: Proper second-order design (needed for xs())
#' # ============================================================
#'
#' # Create a Central Composite Design with 2 factors
#' dsg <- rsm::ccd(2, n0 = c(2,2), alpha = "rotatable", randomize = FALSE)
#'
#' # Simulate a true quadratic response surface with curvature
#' set.seed(123)
#' true_fun <- function(x1, x2) {
#'   2 + 0.5*x1 - 0.3*x2 - 0.4*x1^2 - 0.6*x2^2 + 0.2*x1*x2
#' }
#'
#' dsg$y <- with(dsg, true_fun(x1, x2) + rnorm(nrow(dsg), sd = 0.05))
#'
#' # Run sequential RSM
#' out <- sequential_rsm(dsg, y ~ SO(x1, x2), return_real = FALSE)
#'
#' out$stationary_coded
#' out$prediction
#'
#'
#' # ============================================================
#' # Example 2: coded.data + real-units conversion
#' # ============================================================
#'
#' raw <- rsm::code2val(
#'   dsg,
#'   codings = list(
#'       X1 ~ (x1 * 10) + 50,   # convert coded x1 to real X1
#'       X2 ~ (x2 * 5)  + 30    # convert coded x2 to real X2
#'   )
#' )
#'
#' coded <- rsm::coded.data(raw, x1 ~ (X1 - 50)/10, x2 ~ (X2 - 30)/5)
#'
#' out2 <- sequential_rsm(coded, y ~ SO(x1, x2), return_real = TRUE)
#'
#' out2$stationary_coded
#' out2$stationary_real
#'
#' }
#'
#' @import rsm
#' @importFrom stats predict
#' @export
sequential_rsm <- function(data,
                           formula,
                           return_real = TRUE,
                           desirability = NULL,
                           ...) {

  # --- 1) Fit model with rsm ----
  model <- rsm::rsm(formula, data = data, ...)

  # --- 2) Stationary point in coded units (may throw if model is not quadratic) ----
  stationary <- tryCatch(
    rsm::xs(model),
    error = function(e) {
      stop("Failed to compute stationary point with rsm::xs(). ",
           "Ensure your model supports stationary point extraction (e.g., includes FO/SO terms). ",
           "Original error: ", e$message, call. = FALSE)
    }
  )

  # --- 3) Predict at stationary point ----
  # xs() returns a named numeric vector; turn into data.frame with correct column names
  newdata <- as.data.frame(as.list(stationary))
  pred <- stats::predict(model, newdata = newdata)

  # --- 4) Convert to real units if applicable ----
  stationary_real <- NULL
  if (isTRUE(return_real) && inherits(data, "coded.data")) {
    stationary_real <- tryCatch(
      rsm::code2val(as.data.frame(t(stationary)), codings = rsm::codings(model)),
      error = function(e) NULL
    )
    if (is.data.frame(stationary_real)) {
      # keep as named numeric if single row
      stationary_real <- as.numeric(stationary_real[1, , drop = TRUE])
      names(stationary_real) <- colnames(rsm::code2val(as.data.frame(t(stationary)),
                                                       codings = rsm::codings(model)))
    }
  }

  # --- 5) Optional desirability integration ----
  desir_val <- NULL
  if (!is.null(desirability)) {
    if (is.function(desirability)) {
      desir_val <- desirability(as.numeric(pred))
    } else if (is.list(desirability) && isTRUE(desirability$type == "two-response")) {
      # Convenience: compute overall desirability from IOH/UDI columns if present
      if (!all(c("IOH", "UDI") %in% names(data))) {
        warning("Desirability list provided but data does not contain columns IOH and UDI; skipping.")
      } else {
        if (!requireNamespace("desirability", quietly = TRUE)) {
          warning("Package 'desirability' not available; skipping desirability computation.")
        } else {
          iohD <- desirability::dMin(desirability$ioh_range[1], desirability$ioh_range[2], scale = 1)
          udiD <- desirability::dMax(desirability$udi_range[1], desirability$udi_range[2], scale = 1)
          overall <- desirability::dOverall(iohD, udiD)
          preds <- data.frame(ioh = data$IOH, udi = data$UDI)
          desir_val <- stats::predict(overall, preds)
          desir_val <- as.numeric(desir_val)
        }
      }
    } else {
      warning("Unsupported 'desirability' argument. Provide a function or a list with type='two-response'.")
    }
  }

  # --- 6) Return ----
  list(
    model = model,
    stationary_coded = stationary,
    stationary_real = stationary_real,
    prediction = as.numeric(pred),
    desirability = desir_val
  )
}
