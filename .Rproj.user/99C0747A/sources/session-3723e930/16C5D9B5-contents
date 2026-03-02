#' Bootstrap Confidence Intervals for RSM Stationary Points
#'
#' @param model A fitted rsm model.
#' @param B Number of bootstrap replications.
#'
#' @return A list with bootstrap stationary points and confidence intervals.
#'
#' @import rsm
#' @importFrom stats predict resid model.frame getCall
#' @export
bootstrap_stationary_point <- function(model, B = 1000) {

  # 1) Extract fitted and residuals
  fits  <- stats::predict(model)
  resids <- stats::resid(model)

  # 2) Recover the original data used in the model
  call <- stats::getCall(model)
  dat_name <- as.character(call$data)
  dat_original <- eval(as.name(dat_name), parent.frame())

  # 3) Reconstruct model frame by evaluating terms in original data
  form <- formula(model)
  vars <- all.vars(form)
  dat <- dat_original[, vars, drop = FALSE]

  # Response name
  yname <- vars[1]

  # 4) Bootstrap loop
  boot_raw <- replicate(B, {

    new_y <- fits + sample(resids, replace = TRUE)

    dat_boot <- dat
    dat_boot[[yname]] <- new_y

    mod_boot <- rsm::rsm(form, data = dat_boot)

    rsm::xs(mod_boot)
  })

  boot_df <- as.data.frame(t(boot_raw))

  # 5) Convert to real units if coded.data
  boot_real <- tryCatch(
    rsm::code2val(boot_df, codings = rsm::codings(model)),
    error = function(e) boot_df
  )

  # 6) Confidence intervals
  ci <- lapply(boot_real, function(col)
    stats::quantile(col, c(0.025, 0.975))
  )

  list(
    raw_boot = boot_raw,
    boot = boot_real,
    ci = ci
  )
}
