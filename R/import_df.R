#' Import the Most Recent Dated File from a Directory
#'
#' Searches a directory for files with an embedded 8-digit date in their filename,
#' identifies the most recent file, reads it into R, and returns a cleaned data frame.
#'
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

import_df <- function(directory = "data", 
                      pattern = ".*_(\\d{8})\\..*", 
                      ...) {
  # List all files in the directory (non-full names here)
  all_files <- list.files(directory, full.names = FALSE)
  
  # Filter only files that match the expected pattern
  valid_files <- all_files[grepl(pattern, all_files)]
  if (length(valid_files) == 0) {
    stop("No files with the expected date pattern found in the folder.")
  }
  
  # Extract 8-digit date strings from the filenames
  date_str <- sub(pattern, "\\1", valid_files)
  
  # Convert the extracted strings into Date objects
  date_parsed <- as.Date(date_str, format = "%Y%m%d")
  if (all(is.na(date_parsed))) {
    stop("Date extraction failed. Check your filename pattern and expected format.")
  }
  
  # Identify the file with the most recent date
  most_recent_file <- valid_files[which.max(date_parsed)]
  
  # Construct the full file path (using file.path for cross-platform safety)
  file_path <- file.path(directory, most_recent_file)
  
  # Read the Excel file (pass additional arguments via ...)
  df <- readxl::read_xlsx(file_path, ...)
  
  # Inform the user which file was selected
  message("The most recent file is: ", most_recent_file, 
          " with date: ", as.character(date_parsed[which.max(date_parsed)]))
  
  df = janitor::clean_names(df)
  
  return(df)
}
