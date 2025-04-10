#' Save cleaned import files and updated master lists
#'
#' This function writes the cleaned daily import files and the updated master tracking lists
#' to the appropriate directories. It saves both the daily `master_today` object (with today's data)
#' and the cumulative `master` object, and exports CSV files for REDCap import.
#'
#' @param master_today A named list containing today's processed data, including:
#' \itemize{
#'   \item \code{prior_review}: Data frame of patients previously enrolled (older than 1 week)
#'   \item \code{eligibles}: Data frame of eligible patients 7 days post no-show
#' }
#' @param master A named list representing the updated master record, typically returned by `new_master()`
#' @param date Optional character string in "YYYYMMDD" format. Defaults to today's date.
#'
#' @return This function is called for its side effects and returns no value. It saves:
#' \itemize{
#'   \item `YYYYMMDD_import_priorreviewed.csv` to the `import/` directory
#'   \item `YYYYMMDD_import_new.csv` to the `import/` directory
#'   \item `YYYYMMDD_master.rda` to the `processed_data/` directory
#'   \item the updated `master.rda` file (cumulative) to `processed_data/`
#' }
#'
#' @seealso \code{\link{new_master}}, \code{\link{post_clean}}, \code{\link{run_program}}
#' @export
save_files <- function(master_today, master, date = NULL) {
  # Determine date
  date_actual <- if (is.null(date)) Sys.Date() else as.Date(date, format = "%Y%m%d")
  dateformat <- format(date_actual, "%Y%m%d")

  # Save import files
  write.csv(master_today$prior_review,
            file = paste0("import/", dateformat, "_import_priorreviewed.csv"),
            row.names = FALSE)

  write.csv(master_today$eligibles,
            file = paste0("import/", dateformat, "_import_new.csv"),
            row.names = FALSE)

  # Save RDS files
  saveRDS(master_today, paste0("processed_data/", dateformat, "_master.rda"))
  saveRDS(master, "processed_data/master.rda")
}
