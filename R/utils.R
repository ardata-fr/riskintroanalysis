
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
