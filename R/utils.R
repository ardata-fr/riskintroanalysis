
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
