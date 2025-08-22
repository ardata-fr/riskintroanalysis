
#' Sigmoid function
#'
#' We use the logistic function, rescaled to map non-negative values into [0, 1).
#'
#' The function increases asymptotically up to 1, taking the values .46, .76, .90 and .96
#' at 1, 2, 3 and 4 respectively.
#'
#' @param x Numeric vector.
#'
#' @return A numeric vector with values between -1 and 1.
#' @noRd
#' @examples
#' x <- (0:500)/100
#' plot(x, sigmoid(x), type = 'l')
#' sigmoid(1:5)
#' @importFrom stats plogis
#' @rdname sigmoid
#' @noRd
sigmoid <- function(x) {
  2 * plogis(x) - 1
}

#' Inverse sigmoid function
#'
#' The invesrse of the sigmoid function.
#'
#' @param x Numeric vector with values between -1 and 1.
#'
#' @return A numeric vector with real values.
#' @noRd
#' @examples
#' x <- c(0.01, 0.88, 0.247)
#' inv_sigmoid(x)
#' @importFrom stats qlogis
#' @rdname sigmoid
inv_sigmoid <- function(x) {
  qlogis((x + 1)/2)
}
