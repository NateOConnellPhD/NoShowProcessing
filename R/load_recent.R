#' Load Most Recent Dated Master File
#'
#' Scans the `processed_data/` folder for files matching the `*_master.rda` pattern
#' and loads the one with the most recent date.
#'
#' @return The contents of the most recent dated master RDS file as a list.
#' @export
#'
#' @examples
#' master_today <- load_most_recent_master_today()
#' names(master_today)
load_recent<- function() {
  files <- list.files("processed_data/", pattern = "^[0-9]{8}_master\\.rda$", full.names = TRUE)

  if (length(files) == 0) {
    stop("No dated master files found in 'processed_data/'.")
  }

  # Extract dates from filenames
  dates <- stringr::str_extract(basename(files), "^[0-9]{8}")
  parsed_dates <- as.Date(dates, format = "%Y%m%d")

  # Get file with the latest date
  latest_file <- files[which.max(parsed_dates)]

  message("📂 Loading most recent master_today file: ", basename(latest_file))
  readRDS(latest_file)
}
