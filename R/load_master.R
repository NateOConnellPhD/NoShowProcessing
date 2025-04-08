#' Load Current Master File
#'
#' Reads the main cumulative master list from `processed_data/master.rda`.
#'
#' @return A named list containing the current `master` dataset.
#' @export
#'
#' @examples
#' master <- load_master()
#' names(master)
load_master <- function() {
  path <- "processed_data/master.rda"

  if (!file.exists(path)) {
    stop("master.rda not found in 'processed_data/'.")
  }

  message("📂 Loading current master file: master.rda")
  readRDS(path)
}
