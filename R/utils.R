
# for formatting output numbers in plots mostly
fmt_num <- function(x, ..., replace.na = " - ") {
  case_when(
    is.na(x) ~ replace.na,
    as.integer(x) == x ~ formatC(x, digits = 0, format = "f"),
    TRUE ~ formatC(x, digits = 1, format = "f")
  )
}

# for removing contageous NAs in calculations
rm_na <- function(x, replace_na = 0) {
  if_else(is.na(x), replace_na, x)
}

name_is_valid <- function(string) {
  if (is.null(string)) return(FALSE)
  gsub("[^[:alpha:] ]", "", string) == string
}

cli_abort_if_not <- function(..., .call = .envir, .envir = parent.frame(), .frame = .envir) {
  for (i in seq_len(...length())) {
    if (!all(...elt(i))) {
      cli::cli_abort(
        ...names()[i],
        .call = .call,
        .envir = .envir,
        .frame = .frame
      )
    }
  }
  invisible(NULL)
}


absolute_path <- function(x){

  if (length(x) != 1L)
    stop("'x' must be a single character string")
  epath <- path.expand(x)

  if( file.exists(epath)){
    epath <- normalizePath(epath, "/", mustWork = TRUE)
  } else {
    if( !dir.exists(dirname(epath)) ){
      stop("directory of ", x, " does not exist.", call. = FALSE)
    }
    cat("", file = epath)
    epath <- normalizePath(epath, "/", mustWork = TRUE)
    unlink(epath)
  }
  epath
}

utils::globalVariables(c(".data"))

#' @title copy of HTML from htmltools
#' @description
#' A copy of the `isTruthy` function from the `shiny` package.
#' @param text The text value to mark with HTML
#' @param ... Any additional values to be converted to character and concatenated together
#' @param .noWS Character vector used to omit some of the whitespace that would
#' normally be written around this HTML. Valid options include before, after,
#' and outside (equivalent to before and end).
#' @return A logical value indicating whether the expression is truthy
#' @importFrom rlang dots_list
#' @noRd
HTML <- function (text, ..., .noWS = NULL)
{
  htmlText <- c(text, as.character(dots_list(...)))
  htmlText <- paste8(htmlText, collapse = " ")
  attr(htmlText, "html") <- TRUE
  attr(htmlText, "noWS") <- .noWS
  class(htmlText) <- c("html", "character")
  htmlText
}
paste8 <- function (..., sep = " ", collapse = NULL){
  args <- c(lapply(list(...), enc2utf8), list(sep = if (is.null(sep)) sep else enc2utf8(sep),
                                              collapse = if (is.null(collapse)) collapse else enc2utf8(collapse)))
  do.call(paste, args)
}

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
#' @noRd
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

#' @importFrom glue glue
fmt_scale <- function(scales){
  stopifnot("scale should have length 2" = length(scales) == 2)
  glue::glue("[{scales[[1]]},{scales[[2]]}]")
}



shinyIsRunning <- function () {
  frames <- sys.frames()
  calls <- lapply(sys.calls(), `[[`, 1)
  call_name <- function (call){
    if (is.function(call)) { '<closure>'}
    else {
      deparse(call)
    }
  }
  call_names <- vapply(calls, call_name, character(1))
  target_call <- grep('^runApp$|^shiny::runApp$', call_names)
  isFALSE(length(target_call) == 0)
}



