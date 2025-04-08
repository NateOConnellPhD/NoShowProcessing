#' Load Reference .dta Files from data_check/
#'
#' This function reads all Stata `.dta` files in the `data_check/` folder for a given date prefix
#' and returns them as a named list of data frames.
#'
#' @param date Optional string in "YYYYMMDD" format. If NULL, uses today's date.
#'
#' @return A named list of data frames loaded from `data_check/YYYYMMDD_*.dta`.
#' @export
#'
#' @examples
#' check_data_list <- load_check_data("20250408")
#' names(check_data_list)
#' head(check_data_list$`20250408_all`)
load_check_data <- function(date = NULL) {
  require(haven)

  # Determine date prefix
  today <- if (is.null(date)) Sys.Date() else as.Date(date, format = "%Y%m%d")
  date_string <- format(today, "%Y%m%d")

  # List all .dta files that begin with the date
  dta_files <- list.files("data_check/", pattern = paste0("^", date_string, ".*\\.dta$"), full.names = TRUE)

  if (length(dta_files) == 0) {
    stop("No matching .dta files found in 'data_check/' for date: ", date_string)
  }

  # Load each .dta file into a named list
  data_list <- lapply(dta_files, read_dta)
  names(data_list) <- sub("\\.dta$", "", basename(dta_files))

  return(data_list)
}
