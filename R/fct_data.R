#' Generate a variable correlated with an existing variable
#'
#' Create a numeric vector of the same length as `var` where a share of
#' observations are assigned non-zero values derived from `var`. The function
#' uses normal-score (rank -> normal) transformation to create a latent
#' normal variable that has (approximately) Pearson correlation `corr` with
#' the ranks of `var`, and then scales that latent draw to match the magnitude
#' of a uniform-share scaling of `var`.
#'
#' The algorithm:
#' - samples `share_eligile` fraction of observations as eligible,
#' - splits eligible observations into `length(probs)` quantiles and in each
#'   quantile assigns roughly `probs[i]` share of observations,
#' - for assigned observations builds a target magnitude as `var * U(min_share, max_share)`,
#' - produces a correlated normal score with Pearson correlation `corr` to the
#'   normal scores of the selected `var` values, and maps that back to the
#'   scale (mean/sd) of the target magnitude.
#'
#' Edge cases: `corr` is clamped inside (-0.999, 0.999) for numerical stability;
#' ties in `var` or degenerate groups are handled by falling back to approximate grouping.
#'
#' @param var Numeric vector used as the base variable.
#' @param corr Numeric. Desired Pearson correlation (approx.) between the
#'   generated latent normal score and the normal-score of `var`. Allowed
#'   values in (-1, 1). Negative values produce a reversed relationship.
#' @param probs Numeric vector of length L giving the fraction (0-1) of
#'   observations in each quantile to assign a non-zero generated value.
#'   The function creates L quantiles from the eligible subset.
#' @param share_eligile Numeric in [0,1]. Fraction of total observations sampled
#'   as eligible for assignment.
#' @param min_share Numeric. Lower bound of the uniform draw used to scale
#'   `var` where assigned (multiplier).
#' @param max_share Numeric. Upper bound of the uniform draw used to scale
#'   `var` where assigned (multiplier).
#' @param seed Optional integer seed for reproducible sampling.
#'
#' @return Numeric vector of length `length(var)`. Non-selected observations
#'   are zero. Assigned observations have positive numeric values derived from `var`.
#'
#' @examples
#' set.seed(123)
#' x <- rlnorm(100, 2, .5)
#' y <- gen_var_from_var(x, corr = 0.6, probs = c(0.6, 0.4), share_eligile = 1,
#'                       min_share = 0.1, max_share = 0.5, seed = 42)
#' # approximate correlation between ranks of x and y (non-zero entries)
#' stats::cor(stats::qnorm((rank(x) - 0.5)/length(x)), stats::qnorm((rank(y + 1e-9) - 0.5)/length(y)), use = "complete.obs")
#'
#' @export
gen_var_from_var <- function(
  var,
  corr = 0.5,
  probs = c(0.8, 0.6, 0.4, 0.2, 0.1),
  share_eligile = 1,
  min_share = 0.1,
  max_share = 0.5,
  seed = NULL
) {
  if (!is.null(seed)) set.seed(seed)
  if (!is.numeric(var)) stop("`var` must be numeric")
  n <- length(var)
  if (n == 0) return(numeric(0))

  # prepare output
  out <- numeric(n)

  # number eligible (at least 1 if share_eligile > 0 and n>0)
  n_elig <- max(0, round(n * share_eligile))
  if (n_elig == 0) return(out)

  elig_idx <- sample(seq_len(n), size = n_elig)

  # number of quantiles
  n_qual <- length(probs)
  # safe quantile breaks (handle ties)
  probs_breaks <- seq(0, 1, length.out = n_qual + 1)
  breaks <- tryCatch(
    quantile(var[elig_idx], probs = probs_breaks, na.rm = TRUE),
    error = function(e) rep(NA_real_, length(probs_breaks))
  )

  # if breaks are not strictly increasing (ties) fall back to equal-sized groups
  if (any(is.na(breaks)) || any(diff(breaks) == 0)) {
    # split eligible indices into roughly equal groups
    ord <- order(var[elig_idx])
    groups <- split(elig_idx[ord], rep(1:n_qual, length.out = length(elig_idx)))
    quantiles <- rep(NA_integer_, length(elig_idx))
    for (q in seq_along(groups)) quantiles[match(groups[[q]], elig_idx)] <- q
  } else {
    quantiles <- as.integer(cut(var[elig_idx], breaks = breaks, include.lowest = TRUE, labels = FALSE))
  }

  # clamp corr to valid range for numerical stability
  corr <- as.numeric(corr)
  if (is.na(corr) || corr <= -1) corr <- -0.999
  if (corr >= 1) corr <- 0.999

  # assign within each quantile according to probs
  for (q in seq_len(n_qual)) {
    q_idx <- elig_idx[which(quantiles == q)]
    if (length(q_idx) == 0) next
    n_in_q <- length(q_idx)
    n_assign <- round(n_in_q * probs[q])
    if (n_assign <= 0) next
    sel <- sample(q_idx, n_assign)

    base_vals <- var[sel]

    # create a target magnitude using uniform shares
    shares <- runif(length(sel), min = min_share, max = max_share)
    target_raw <- base_vals * shares

    # if only one value selected, assign target_raw directly
    if (length(sel) <= 1) {
      out[sel] <- target_raw
      next
    }

    # normal-score transform (ranks -> uniform -> normal) of the selected base
    r <- rank(base_vals, ties.method = "average")
    z <- stats::qnorm((r - 0.5) / length(r))

    # generate correlated normal score
    eps <- stats::rnorm(length(r))
    z_new <- corr * z + sqrt(1 - corr^2) * eps

    # map z_new to the scale of target_raw (preserve mean & sd of target_raw)
    if (stats::sd(z_new) <= 0 || stats::sd(target_raw) <= 0) {
      out[sel] <- target_raw
    } else {
      out[sel] <- (z_new - mean(z_new)) / stats::sd(z_new) * stats::sd(target_raw) + mean(target_raw)
    }
  }

  out
}


#' Check variables by income deciles
#'
#' Create deciles (or other `n`) based on an income variable and return
#' aggregated statistics for a set of variables by those deciles.
#'
#' This is a convenience wrapper around `calc_deciles()` and `calc_agg_by()`.
#' You can supply variables as bare vectors (e.g. `check_vars_by_deciles(ym, yp)`)
#' or pass a data frame and use tidy-select (e.g. `check_vars_by_deciles(ym, yp, data = dta_hh)`).
#'
#' @param ... Bare variable names or tidy-select expressions. If `data` is NULL
#'   the expressions are evaluated to create a tibble; if `data` is provided
#'   they are used to select columns from `data`.
#' @param data Optional data frame to select variables from.
#' @param n Integer number of quantiles (deciles = 10). Must be >= 2.
#' @param dec_var Character name of the income variable used to form deciles.
#'   Defaults to `get_inc_nm(suffix = NULL)$var`.
#' @param wt_var Character name of the weight variable passed to the helper
#'   functions. Defaults to `get_wt_nm()`.
#'
#' @return A tibble produced by `calc_agg_by()` (aggregated stats by decile).
#'
#' @examples
#' # bare vectors in the calling environment
#' # check_vars_by_deciles(ym, ben_pension, ben_unemployment)
#'
#' # using a data frame
#' # check_vars_by_deciles(ym, ben_pension, data = dta_hh, n = 10)
#'
#' @export
check_vars_by_deciles <- function(...,
                                 data = NULL,
                                 n = 10,
                                 dec_var = get_inc_nm(suffix = NULL)$var,
                                 wt_var = get_wt_nm()) {
  quos_vars <- rlang::enquos(...)

  if (!is.numeric(n) || length(n) != 1 || n < 2) {
    rlang::abort("`n` must be a single numeric value >= 2")
  }

  # Build the data.frame: either from provided `data` or from the supplied expressions
  if (is.null(data)) {
    if (length(quos_vars) == 0) {
      rlang::abort("No variables supplied. Provide variables via `...` or supply `data` and use tidy-select.")
    }
    dta <- tibble::tibble(!!!quos_vars)
  } else {
    if (!is.data.frame(data)) {
      rlang::abort("`data` must be a data frame or tibble")
    }
    dta <- if (length(quos_vars) == 0) {
      data
    } else {
      dplyr::select(data, !!!quos_vars)
    }
  }

  # Ensure dec_var exists in the environment used by calc_deciles (it may refer to a column name)
  # We pass dta into calc_deciles which expects the income variable name to exist in that data.
  result <- dta |>
    calc_deciles(
      dec_var = dec_var,
      wt_var = wt_var,
      n_dec = as.integer(n)
    ) |>
    calc_agg_by(
      vars = get_var_nm(suffix = NULL)$var,
      by_var = "ym_decile",
      wt_var = wt_var
    )

  result
}