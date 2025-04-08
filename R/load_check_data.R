#' Load All .dta Reference Files from data_check/
#'
#' This function loads all `.dta` files from the `data_check/` folder, including both
#' files prefixed with a date (e.g., "20250408_all.dta") and static filenames
#' (e.g., "eligibles.dta", "full_import_list.dta").
#'
#' If a `date` is supplied, only date-prefixed files from that date and the non-date files
#' will be returned. If NULL, all `.dta` files are loaded regardless of prefix.
#'
#' @param date Optional string in "YYYYMMDD" format. If supplied, loads only that day's files
#'             plus any non-prefixed .dta files. If NULL, loads all .dta files in folder.
#'
#' @return A named list of data frames.
#' @export
#'
#' @examples
#' files <- load_check_data("20250408")
#' names(files)
#' head(files$`20250408_all`)

load_check_data <- function(date = NULL) {
  require(haven)

  # Build pattern to match files
  if (!is.null(date)) {
    prefix_pattern <- paste0("^(", date, "_.*|[^0-9].*)\\.dta$")
  } else {
    prefix_pattern <- "\\.dta$"  # Load everything
  }

  # List matching .dta files
  dta_files <- list.files("data_check/", pattern = prefix_pattern, full.names = TRUE)

  if (length(dta_files) == 0) {
    stop("No matching .dta files found in 'data_check/'", if (!is.null(date)) paste(" for date:", date))
  }

  # Load and name
  data_list <- lapply(dta_files, read_dta)
  names(data_list) <- sub("\\.dta$", "", basename(dta_files))

  return(data_list)
}
