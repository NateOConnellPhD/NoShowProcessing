#' Import the Most Recent Dated File from a Directory
#'
#' Searches a directory for files with an embedded 8-digit date in their filename,
#' identifies the most recent file, reads it into R, and returns a cleaned data frame.
#'
#' @param date A date variable in the format of "YYYYMMDD"; if not supplied, finds most recent file
#' @param directory A character string specifying the path to the folder containing the files.
#'   Defaults to `"data"`.
#' @param pattern A regular expression for matching filenames with a date. The default
#'   captures an 8-digit date (YYYYMMDD) preceded by an underscore and followed by a file extension.
#' @param ... Additional arguments passed to `readxl::read_xlsx()` for file import.
#'
#' @return A data frame containing the contents of the most recent dated file, with
#'   cleaned column names via `janitor::clean_names()`.
#'
#' @details
#' This function:
#' \enumerate{
#'   \item Lists files in the specified `directory`.
#'   \item Filters files matching the specified `pattern`.
#'   \item Extracts the date component from filenames.
#'   \item Converts those date strings to `Date` objects.
#'   \item Selects the file with the most recent date.
#'   \item Reads that file using `readxl::read_xlsx()`.
#'   \item Cleans the column names using `janitor::clean_names()`.
#' }
#'
#' @examples
#' \dontrun{
#' latest_data <- import_df("my_data_folder")
#' head(latest_data)
#' }
#'
#' @export
import_df <- function(date = NULL,
                      directory = "data",
                      pattern = ".*_(\\d{8})\\..*",
                      ...) {
  # List all files in the directory
  all_files <- list.files(directory, full.names = FALSE)

  # Filter only files that match the expected pattern
  valid_files <- all_files[grepl(pattern, all_files)]
  if (length(valid_files) == 0) {
    stop("No files with the expected date pattern found in the folder.")
  }

  # Extract 8-digit date strings from the filenames
  date_strs <- sub(pattern, "\\1", valid_files)

  # Convert the extracted strings into Date objects
  parsed_dates <- as.Date(date_strs, format = "%Y%m%d")
  if (all(is.na(parsed_dates))) {
    stop("Date extraction failed. Check your filename pattern and expected format.")
  }

  # Determine which file to use
  if (!is.null(date)) {
    # Allow date as Date or character "YYYYMMDD"
    target_date <- if (inherits(date, "Date")) {
      date
    } else {
      as.Date(as.character(date), format = "%Y%m%d")
    }
    if (is.na(target_date)) {
      stop("Provided date could not be parsed. Use Date or 'YYYYMMDD' string.")
    }
    match_idx <- which(parsed_dates == target_date)
    if (length(match_idx) == 0) {
      stop("No file found for date ", format(target_date, "%Y%m%d"))
    }
    selected_file <- valid_files[match_idx[1]]
    selected_date <- parsed_dates[match_idx[1]]
    message("Loaded file for specified date: ", selected_file,
            " (", format(selected_date, "%Y-%m-%d"), ")")
  } else {
    # Fallback to the most recent file
    rec_idx <- which.max(parsed_dates)
    selected_file <- valid_files[rec_idx]
    selected_date <- parsed_dates[rec_idx]
    message("Loaded most recent file: ", selected_file,
            " (", format(selected_date, "%Y-%m-%d"), ")")
  }

  # Construct the full file path
  file_path <- file.path(directory, selected_file)

  # Read the Excel file and clean column names
  df <- readxl::read_xlsx(file_path, ...)
  df <- janitor::clean_names(df)

  return(df)
}
