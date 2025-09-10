
#' Effective number of sources of an entry point
#'
#' Equivalent number of sources of maximum emisison score.
#'
#' Each source contributes a quantity between 0 and 1, equal to the ratio between its
#' emission score and the maximum possible score.
#'
#' @param x Numeric vector. Risk of emission scores of source countries.
#' @param x_max Positive number. Maximum possible emission score for a country
#' @export
#' @examples
#' n_eff_sources(c(12, 12), x_max = 12)  # 2 equivalent sources
#' n_eff_sources(c(6, 6), x_max = 12)    # 2 half-score sources = 1 equivalent source
n_eff_sources <- function(x, x_max) {
  stopifnot(
    length(x_max) == 1,
    x_max > 0
  )
  sum(x) / x_max
}


#' Scale risk of entry points
#'
#' Scale the effective numbers of legal and illegal sources into the risk scale.
#'
#' @param x_legal Numeric vector. Effective numbers of sources for each legal entry point.
#' @param x_illegal Numeric vector. Effective numbers of sources for each illegal entry point.
#' @param illegal_factor Number > 1. Relative risk of an illegal entry point with respect
#'   to a legal one.
#' @param coef_legal Number > 1. Scaling factor of legal sources in the latent scale.
#' @param coef_illegal Number > 1. Scaling factor of illegal sources in the latent scale.
#' @param max_risk Number > 1. Maximum assymptotically atteinable risk.
#' @examples
#' library(ggplot2)
#' library(tidyr)
#' case_dat <- tidyr::expand_grid(
#'   xl = 0:5,
#'   xi = 0:5
#' )
#' case_dat |>
#'   transform(
#'     risk = scale_entry_points(xl, xi, 3, 1, 1, 100)
#'   ) |>
#'   ggplot(aes(xl, xi, fill = risk)) +
#'   geom_raster() +
#'   coord_fixed() +
#'   scale_fill_viridis_c(option = "D") +
#'   labs(
#'     x = "N legal eps",
#'     y = "N illegal eps",
#'     fill = "Risk"
#'   )
#' @export
scale_entry_points <- function(x_legal, x_illegal, illegal_factor, coef_legal, coef_illegal, max_risk) {

  stopifnot(
    length(illegal_factor) == 1,
    length(coef_legal) == 1,
    length(coef_illegal) == 1,
    length(max_risk) == 1,
    all(x_legal >= 0),
    all(x_illegal >= 0),
    illegal_factor > 1,
    coef_legal > 0,
    coef_illegal > 0,
    max_risk > 0
  )

  ## Transform the effective number of legal sources into the risk scale,
  ## apply the illegal scaling factor inversely and back-transform to the latent scale
  latent_legal_risk <- inv_sigmoid(sigmoid(coef_legal * x_legal) / illegal_factor)

  ## Combine with the effective number of illegal sources
  latent_risk <- latent_legal_risk + coef_illegal * x_illegal

  ## Scale into the risk scale
  ans <- max_risk * sigmoid(latent_risk)

  return(ans)
}

#' Test whether a vector is boolean
#'
#' Either logical vectors or vectors with only 0 and 1 entries count as boolean.
#'
#' @param x A vector.
#' @export
#' @examples
#' is.boolean(c(TRUE, TRUE, FALSE))
#' is.boolean(c(0, 1, 1, 0))
#' is.boolean(0)
#' is.boolean(c(0, 1.1, 3))
#' is.boolean(c("0", "1"))
is.boolean <- function(x) {
  if (is.logical(x)) return(TRUE)
  if (!is.numeric(x)) return(FALSE)
  sum(x == 0) + sum( (x - 1) == 0) == length(x)
}



