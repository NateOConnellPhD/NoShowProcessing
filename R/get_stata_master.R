#' Create Stata-Based Master Dataset for a Specific Date
#'
#' This function reads in Stata `.dta` files and an Excel file for a specified date
#' and builds a master list object with processed, eligible, and ineligible patient data.
#'
#' If `date` is not supplied, it uses the most recent weekday:
#' - If today is Monday, it uses the previous Friday.
#' - Otherwise, it uses yesterday.
#'
#' @param date Optional string in "YYYYMMDD" format. If NULL, selects the most recent weekday automatically.
#' @param overwrite_master Logical. If TRUE, will overwrite the existing `master.rda` file.
#'
#' @return Saves two RDS files:
#' \itemize{
#'   \item `processed_data/YYYYMMDD_master.rda`: a date-specific daily master
#'   \item `processed_data/master.rda`: the cumulative master (only if overwrite is allowed)
#' }
#' @export
create_stata_master <- function(date = NULL, overwrite_master = FALSE) {
  # Determine most recent weekday if date is not supplied
  if (is.null(date)) {
    today <- Sys.Date()
    dow <- lubridate::wday(today, week_start = 1) # Monday = 1
    recent_date <- if (dow == 1) today - 3 else today - 1
    date <- format(recent_date, "%Y%m%d")
  }

  # Define file paths
  data_folder <- "stata_data/"
  xlsx_file <- paste0("stata_data/datarequest_1585_", date, ".xlsx")
  master_file <- "processed_data/master.rda"
  dated_master_file <- paste0("processed_data/", date, "_master.rda")

  # Read .dta files
  dta_files <- list.files(path = data_folder, pattern = "\\.dta$", full.names = TRUE)
  data_list <- lapply(dta_files, haven::read_dta)
  names(data_list) <- sub("\\.dta$", "", basename(dta_files))

  # Read Excel data
  df_old <- readxl::read_xlsx(xlsx_file)

  # Build master_today
  master_today <- list()
  master_today$df <- df_old
  master_today$df_proc <- data_list[[paste0(date, "_all")]]
  master_today$df_7day <- data_list[[paste0(date, "_7dayvisits")]]
  master_today$inel <- list(
    lang = data_list[[paste0(date, "_ineligibles_lang")]],
    nophone = data_list[[paste0(date, "_ineligibles_nophone")]],
    resched = data_list[[paste0(date, "_ineligibles_resched")]]
  )
  master_today$eligibles <- data_list[[paste0(date, "_import_new")]]
  master_today$prior_combine <- data_list[[paste0(as.character(as.numeric(date) - 1), "_import_combined")]]
  master_today$prior_review <- data_list[[paste0(date, "_import_priorreviewed")]]

  # Save daily master file (always)
  saveRDS(master_today, file = dated_master_file)
  message("✅ Saved daily master as: ", dated_master_file)

  # Build and optionally save cumulative master
  if (file.exists(master_file) && !overwrite_master) {
    message("⚠️  master.rda already exists. Skipping overwrite. Use `overwrite_master = TRUE` to overwrite it.")
  } else {
    master <- list(
      eligibles = data_list$eligibles,
      inel = list(
        lang = data_list$ineligibles_lang,
        nophone = data_list$ineligibles_nophone,
        resched = data_list$ineligibles_resched
      ),
      full = data_list$full_import_list
    )
    saveRDS(master, file = master_file)
    message("✅ Saved updated cumulative master as: ", master_file)
  }

  return(invisible("Stata master creation complete."))
}
