#' Set Riskintro working directory
#'
#' Once set, the RiskIntro will automatically open the set working directory. It sets the
#' global variable named `riskintro_workspace_dir` which the application checks when opened.
#'
#' Intended mainly for developer use.
#'
#' @param wd directory path to the folder containg the Riskintro working directory.
#'
#' @returns returns nothing
#' @export
#'
#' @examples
#' \dontrun{
#' riskintro::set_wd("~/test-study")
#' }
set_wd <- function(wd) {
  awd <- absolute_path(wd)
  settings <- file.path(wd, "settings.yaml")
  if (!dir.exists(awd)) cli::cli_abort("That directory doesn't exist!")
  options(riskintro_workspace_dir = awd)
  cli::cli_alert_success("RiskIntro working directory set to: `{awd}`")
  if (file.exists(settings)) {
    cli::cli_alert_info(
      "This directory has {.file {settings}} file from which settings will be loaded when RiskIntro starts."
    )
  } else {
    cli::cli_alert_info(
      "This directory has no `settings.yaml` file. If that is expected, please continue."
    )
  }
}

globalVariables(names = c(".data", "dataContext"))

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

#' @importFrom gdtools register_liberationsans
#' @import s2 promises future
.onLoad <- function(libname, pkgname) {
  suppressMessages(sf_use_s2(FALSE))

  if (requireNamespace("rgeoboundaries", quietly = TRUE)) {
    rgeoboundaries::init_rgeoboundaries_cache()
  }
}

name_is_valid <- function(string) {
  if (is.null(string)) return(FALSE)
  gsub("[^[:alpha:] ]", "", string) == string
}
