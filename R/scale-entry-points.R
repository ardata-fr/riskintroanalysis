
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



