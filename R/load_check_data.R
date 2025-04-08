#' Load Most Recent Check Data from `data_check/` (with Import Combined Logic)
#'
#' Loads the most recent date-prefixed `.dta` files (only one per suffix), except for
#' `*_import_combined.dta` which always pulls from the previous weekday.
#' Also includes all static `.dta` files (no date prefix).
#'
#' @return Named list of data frames.
#' @export
load_check_data <- function() {
  require(haven)
  require(stringr)
  require(dplyr)
  require(lubridate)

  files <- list.files("data_check/", pattern = "\\.dta$", full.names = TRUE)
  if (length(files) == 0) stop("No .dta files found in 'data_check/'.")

  # Parse filenames
  file_info <- tibble::tibble(
    path = files,
    file = basename(files),
    match = str_match(basename(files), "^(\\d{8})_(.+)\\.dta$")
  ) %>%
    mutate(
      has_date = !is.na(match[, 1]),
      date = if_else(has_date, as.Date(match[, 2], format = "%Y%m%d"), as.Date(NA)),
      suffix = if_else(has_date, match[, 3], NA_character_),
      name = if_else(has_date, paste0(match[, 2], "_", match[, 3]), tools::file_path_sans_ext(file))
    )

  today <- Sys.Date()
  prev_weekday <- today - ifelse(wday(today) == 2, 3, 1)  # Monday -> Friday logic
  prev_string <- format(prev_weekday, "%Y%m%d")

  # Split files
  dated <- file_info %>% filter(has_date)
  static <- file_info %>% filter(!has_date)

  # For all suffixes EXCEPT "import_combined", grab the most recent by suffix
  latest_dated <- dated %>%
    filter(suffix != "import_combined") %>%
    group_by(suffix) %>%
    filter(date == max(date)) %>%
    ungroup()

  # For import_combined: pull only the one from the correct previous weekday
  import_combined <- dated %>%
    filter(suffix == "import_combined", date == prev_weekday)

  # Combine dated + static file rows
  final_files <- bind_rows(latest_dated, import_combined, static)

  # Load all selected files
  result <- purrr::set_names(
    lapply(final_files$path, haven::read_dta),
    final_files$name
  )

  return(result)
}
