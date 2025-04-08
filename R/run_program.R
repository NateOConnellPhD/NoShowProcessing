#' Run the full daily no-show trial data pipeline
#'
#' This function performs the full daily data processing workflow for the no-show trial.
#' It reads in the most recent master files, processes new data, identifies eligibles
#' and ineligibles, merges with prior data, performs cleaning steps, and saves all
#' relevant output files.
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
#'   \item Saves updated outputs to disk
#' }
#'
#' @return No return value. Saves processed data to disk.
#' @export
run_program <- function() {
  message("🔄 Starting daily no-show trial data pipeline...")

  # Load latest master_prev based on file naming pattern
  master_files <- list.files("processed_data/", pattern = "_master\\.rda$", full.names = TRUE)
  file_dates <- stringr::str_extract(master_files, "\\d{8}")  # match YYYYMMDD
  latest_index <- which.max(as.Date(file_dates, format = "%Y%m%d"))
  master_prev <- readRDS(master_files[latest_index])
  message("✅ Loaded most recent previous master file: ", basename(master_files[latest_index]))

  # Load current master
  master <- readRDS("processed_data/master.rda")
  message("✅ Loaded current master file")

  ##### Set up today's list ############
  master_today <- list()

  ###### Import data ##########
  message("📥 Importing today's raw data...")
  df <- import_df()

  ###### Process Data #########
  message("🔧 Processing today's raw data...")
  df_proc <- process_df(df)
  master_today$df_proc <- df_proc
  message("✅ Processed data frame added to master_today$df_proc")

  ###### Get 7-day data #########
  message("📆 Extracting no-show data from 7 days ago...")
  df_7day <- get_7_day(df_proc)
  master_today$df_7day <- df_7day
  message("✅ 7-day no-show data added to master_today$df_7day")

  ###### Get ineligibles ########
  message("🚫 Identifying ineligible patients...")
  df_inels <- get_inels(df_7day)
  master_today$inel <- df_inels
  message("✅ Ineligible data stored in master_today$inel")

  ######### Get eligibles ###########
  message("✅ Identifying eligible patients for import...")
  df_els <- get_eligible(df_7day)
  master_today$eligibles <- df_els
  message("✅ Eligible patients stored in master_today$eligibles")

  ###### Merge and clean ###########
  message("🔗 Merging today's data with prior reviewed data...")
  df_merge <- merge_and_clean(master_today, master_prev)
  master_today$prior_combine <- df_merge$prior_combined
  master_today$prior_review <- df_merge$prior_review
  message("✅ Merged data added to master_today$prior_combine and $prior_review")

  ########## Post-cleaning ##############
  message("🧹 Running post-cleaning steps...")
  new_master <- new_master(master_today)
  message("✅ Full import list created and stored in new_master")

  ######### Save files #############
  message("💾 Saving updated files to disk...")
  save_files(master_today, master)
  message("🎉 Pipeline complete. All files saved.")
}
