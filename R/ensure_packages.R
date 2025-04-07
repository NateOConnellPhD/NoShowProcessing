#' Ensure Required R Packages Are Installed and Loaded
#'
#' This function checks whether specified R packages are installed. If a package
#' is not installed, it will be installed from CRAN. After confirming installation,
#' the package is loaded using \code{library()}.
#'
#' This is useful to ensure a consistent working environment for scripts or packages
#' that rely on specific dependencies.
#'
#' @param pkgs A character vector of package names to check, install if necessary, and load.
#'
#' @return Invisibly returns \code{TRUE} after all packages have been loaded.
#' @export
#'
#' @examples
#' ensure_packages(c("dplyr", "haven", "lubridate"))
#' ensure_packages(c("devtools", "tidyverse"))
ensure_packages <- function(pkgs) {
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message("Installing package: ", pkg)
      install.packages(pkg)
    }
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
  invisible(TRUE)
}
