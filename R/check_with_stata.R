#' Cross-Check R Pipeline Against STATA Output
#'
#' This function runs the full no-show trial data processing pipeline for a given date
#' and cross-checks each key output against corresponding STATA-generated `.dta` files
#' in the `data_check/` directory.
#'
#' It reconstructs the daily pipeline in-memory, using helper functions like
#' `import_df()`, `process_df()`, `get_7_day()`, `get_inels()`, `get_eligible()`,
#' `merge_and_clean()`, and `new_master()`, and compares all output files against
#' their expected `.dta` equivalents using `compare_datasets()`.
#'
#' @param date Optional character string in \code{"YYYYMMDD"} format.
#'   If NULL, defaults to today's date.
#'
#' @return Invisibly returns a message indicating whether all datasets match.
#'   If mismatches are found, a warning is issued and the mismatched dataset names are printed.
#'
#' @details
#' The function expects that comparison `.dta` files for the specified date
#' exist in the `data_check/` folder and are named consistently using the pattern:
#' \itemize{
#'   \item `YYYYMMDD_all.dta`
#'   \item `YYYYMMDD_7dayvisits.dta`
#'   \item `YYYYMMDD_ineligibles_lang.dta`
#'   \item `YYYYMMDD_ineligibles_resched.dta`
#'   \item `YYYYMMDD_ineligibles_nophone.dta`
#'   \item `YYYYMMDD_import_new.dta`
#'   \item `YYYYMMDD_import_priorreviewed.dta`
#'   \item `YYYYMMDD_import_combined.dta` (uses prior weekday’s date)
#'   \item `full_import_list.dta`, `eligibles.dta`, `ineligibles_*.dta` (final output checks)
#' }
#'
#' @seealso [run_program()], [crosscheck_data()], [compare_datasets()]
#'
#' @examples
#' \dontrun{
#' check_with_stata("20250407")
#' check_with_stata() # Defaults to today
#' }
#'
#' @export

check_with_stata <- function(date = NULL) {
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
  df_merge <- merge_and_clean(master_today, master_prev)
  master_today$prior_combine <- df_merge$prior_combined
  master_today$prior_review <- df_merge$prior_review

  message("🧹 Running post-cleaning...")
  new_master <- new_master(master_today)

  message("🔍 Running cross-checks against reference files...")
  check <- crosscheck_data(master_today, new_master, date = date_string)

  if (!isTRUE(check)) {
    warning("❌ One or more datasets do not match: ", paste(check, collapse = ", "))
    return(invisible("❌ Datasets do not match.)")
  } else{
    return(invisible("✅ All datasets match."))
  }

}
