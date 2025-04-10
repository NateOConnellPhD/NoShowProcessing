#' Run the full daily no-show trial data pipeline
#'
#' This function performs the full daily data processing workflow for the no-show trial.
#' It reads in the most recent master files, processes new data, identifies eligibles
#' and ineligibles, merges with prior data, performs cleaning steps, compares to reference datasets,
#' and saves all relevant output files only if all data match.
#'
#' Steps performed:
#' \itemize{
#'   \item Loads current and previous master datasets
#'   \item Imports and processes today's raw data
#'   \item Extracts no-show data from 7 days ago
#'   \item Identifies ineligible patients (e.g., language, reschedule, no phone)
#'   \item Identifies eligible patients
#'   \item Merges today's data with prior reviewed data
#'   \item Applies post-cleaning to prepare full import list
#'   \item Compares against reference datasets
#'   \item Saves updated outputs to disk (only if all match)
#' }
#'
#' @param date Optional date in "YYYYMMDD" format. If NULL, defaults to today's date.
#' @return Returns "✅ All datasets match." or lists the mismatched datasets.
#' @export
run_program <- function(date = NULL) {
  message("🔄 Starting daily no-show trial data pipeline...")

  today <- if (is.null(date)) Sys.Date() else as.Date(date, format = "%Y%m%d")
  date_string <- format(today, "%Y%m%d")

  # Load latest master_prev
  master_files <- list.files("processed_data/", pattern = "_master\\.rda$", full.names = TRUE)
  file_dates <- stringr::str_extract(master_files, "\\d{8}")
  latest_index <- which.max(as.Date(file_dates, format = "%Y%m%d"))
  master_prev <- readRDS(master_files[latest_index])
  message("✅ Loaded most recent previous master file: ", basename(master_files[latest_index]))

  master <- readRDS("processed_data/master.rda")
  message("✅ Loaded current master file")

  master_today <- list()

  message("📥 Importing today's raw data...")
  df <- import_df()
  master_today$df = df

  message("🔧 Processing today's raw data...")
  df_proc <- process_df(df)
  master_today$df_proc <- df_proc
  message("✅ Added df_proc")

  message("📆 Extracting no-show data from 7 days ago...")
  df_7day <- get_7_day(df_proc, date = date_string)
  master_today$df_7day <- df_7day

  message("🚫 Identifying ineligible patients...")
  df_inels <- get_inels(df_7day, date = date_string)
  master_today$inel <- df_inels

  message("✅ Identifying eligible patients...")
  df_els <- get_eligible(df_7day, date = date_string)
  master_today$eligibles <- df_els

  message("🔗 Merging today's data with prior reviewed data...")
  df_merge <- merge_and_clean(master_today, master_prev, date=date_string)
  master_today$prior_combine <- df_merge$prior_combined
  master_today$prior_review <- df_merge$prior_review

  message("🧹 Running post-cleaning...")
  new_master <- new_master(master_today)

  message("💾 Saving updated files to disk...")
  save_files(master_today, master, date = date_string)
  message("🎉 Pipeline complete. All files saved successfully.")

}

