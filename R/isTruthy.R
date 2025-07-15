#' @title Truthy and falsy values
#' @description
#' A copy of the `isTruthy` function from the `shiny` package.
#' @param x An expression whose truthiness value we want to determine
#' @return A logical value indicating whether the expression is truthy
#' @examples
#' isTruthy(1)
#' isTruthy(character())
#' isTruthy(character(1))
#' isTruthy(NULL)
#' @export
isTruthy <- function (x) {
  if (is.null(x))
    return(FALSE)
  if (inherits(x, "try-error"))
    return(FALSE)
  if (!is.atomic(x))
    return(TRUE)
  if (length(x) == 0)
    return(FALSE)
  if (all(is.na(x)))
    return(FALSE)
  if (is.character(x) && !any(nzchar(stats::na.omit(x))))
    return(FALSE)
  if (inherits(x, "shinyActionButtonValue") && x == 0)
    return(FALSE)
  if (is.logical(x) && !any(stats::na.omit(x)))
    return(FALSE)
  return(TRUE)
}

