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
save_files <- function(master_today, master, master_old, date = NULL) {
  # Determine date
  date_actual <- if (is.null(date)) Sys.Date() else as.Date(date, format = "%Y%m%d")
  dateformat <- format(date_actual, "%Y%m%d")

  #exclude key variables for import
  excVars = c("completedwcvdate", "completedwcvtype", "diff_next_wcv_date",
             "diff_next_wcv_entry_date", "diff_next_wcv_type")

  prior_review_temp <- master_today$prior_review %>%
    select(-all_of(excVars))

  prior_review_temp <- prior_review_temp %>%
    # move redcap_repeat_instance so it comes *after* the 3rd column (i.e. into slot 4)
    relocate(redcap_repeat_instance, .after = 3)

  today = master_today$eligibles

  #edit timestamps
  today$timenoshow_24hr <- sub("^\\s", "", format(today$timenoshow_24hr, "%k:%M"))
  today$timenoshow <- sub("^0", "", format(today$timenoshow, "%I:%M %p"))

  prior_review_temp$timenoshow_24hr <- sub("^\\s", "", format(prior_review_temp$timenoshow_24hr, "%k:%M"))
  prior_review_temp$timenoshow <- sub("^0", "", format(prior_review_temp$timenoshow, "%I:%M %p"))


  # Function to turn all columns to character and blank‐out NAs
  blank_na_cols <- function(df) {
    df[] <- lapply(df, function(col) {
      col <- as.character(col)
      col[is.na(col)] <- ""
      col
    })
    df
  }

  # Apply to both data frames
  today              <- blank_na_cols(today)
  prior_review_temp  <- blank_na_cols(prior_review_temp)

  ### process previously saved data with language and site already in the system
  today$language = ifelse(today$twilio_phone %in% master_old$full$twilio_phone, "",today$language)
  prior_review_temp$language = ifelse(prior_review_temp$twilio_phone %in% master_old$full$twilio_phone, "", prior_review_temp$language)
  today$site= ifelse(today$twilio_phone %in% master_old$full$twilio_phone, "",today$site)
  prior_review_temp$site = ifelse(prior_review_temp$twilio_phone %in% master_old$full$twilio_phone, "", prior_review_temp$site)

  prior_review_temp$nextwcvdate <- prior_review_temp$nextwcvdate |>
    as.POSIXct(format = "%Y-%m-%d %H:%M") |>
    floor_date(unit = "minute") |>
    format("%Y-%m-%d %I:%M")


  prior_review_temp$next_wcv_entry_date <- floor_date(
    as.POSIXct(prior_review_temp$next_wcv_entry_date, format = "%Y-%m-%d %H:%M"),
    unit = "minute"
  )

  # Save import files
  write.csv(prior_review_temp,
            file = paste0("import/", dateformat, "_import_priorreviewed.csv"),
            row.names = FALSE)

  write.csv(today,
            file = paste0("import/", dateformat, "_import_new.csv"),
            row.names = FALSE)

  # Save RDS files
  saveRDS(master_today, paste0("processed_data/", dateformat, "_master.rda"))
  saveRDS(master, "processed_data/master.rda")
}
