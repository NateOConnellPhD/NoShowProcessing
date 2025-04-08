#' Merge and Clean Daily and Prior Data
#'
#' This function merges the current day's processed dataset with prior days' eligible and reviewed data,
#' filters and cleans the merged dataset, reconciles discrepancies in key WCV fields, and outputs a cleaned
#' version ready for review or export.
#'
#' @param master_today A named list containing today's datasets. Must include `df_proc`, the day's full processed data.
#' @param master_prev A named list containing prior datasets. Must include `eligibles` and `prior_review` from the previous day (typically Friday).
#' @param date Optional date string in "YYYYMMDD" format. Defaults to today's date.
#'
#' @return A named list with two data frames:
#' \describe{
#'   \item{`prior_review`}{Cleaned and reconciled data from today, suitable for REDCap re-upload.}
#'   \item{`prior_combined`}{Raw merged prior data from `eligibles` and `prior_review`.}
#' }
#'
#' @export

merge_and_clean <- function(master_today, master_prev, date = NULL) {
  # Parse date input
  today_date <- if (is.null(date)) Sys.Date() else as.Date(date, format = "%Y%m%d")

  # Use current day's full dataset
  df_today_all <- master_today$df_proc

  # Filter: drop if language is not English or Spanish
  df_today_all <- df_today_all %>%
    dplyr::filter(language %in% c("en", "es"))

  # Drop newly eligible and later visits (no-shows must be >= 7 days ago)
  df_today_all <- df_today_all %>%
    dplyr::filter(datenoshow < today_date - 7)

  # Combine prior eligible and reviewed data
  prior_combined <- dplyr::bind_rows(
    master_prev$eligibles,
    master_prev$prior_review
  )

  # Standardize variable names for join
  names(prior_combined)[c(5,12:20,27:28,30:31)] <- c(
    "cellphone", "datenoshow", "timenoshow_24hr", "timenoshow",
    "deptexternalname", "visittypename", "visitstatus", "visitexternalname",
    "ageinyears", "ageinmonths", "nextwcvdate", "nextwcvtype",
    "prior2yrwcvnoshow", "prior2yrvisitnoshow"
  )

  # Merge by key identifiers
  df_merged <- df_today_all %>%
    dplyr::left_join(
      prior_combined %>%
        dplyr::select(home_phone, pat_first_name, datenoshow, timenoshow, redcap_repeat_instance),
      by = c("home_phone", "pat_first_name", "datenoshow", "timenoshow")
    ) %>%
    dplyr::filter(!is.na(redcap_repeat_instance))

  # Add source flag and stack for diff tracking
  comparison_df <- dplyr::bind_rows(
    df_merged %>% mutate(.source = "today"),
    prior_combined %>% mutate(.source = "prior")
  )

  # Identify discrepancies and reconcile fields
  comparison_df <- comparison_df %>%
    dplyr::arrange(home_phone, pat_first_name, datenoshow, desc(.source)) %>%
    dplyr::group_by(home_phone, pat_first_name, datenoshow) %>%
    dplyr::mutate(
      diff_next_wcv_date = dplyr::n_distinct(nextwcvdate) > 1,
      diff_next_wcv_entry_date = dplyr::n_distinct(next_wcv_entry_date) > 1,
      diff_next_wcv_type = dplyr::n_distinct(nextwcvtype) > 1
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(home_phone, pat_first_name, datenoshow) %>%
    dplyr::mutate(
      nextwcvdate = coalesce(
        nextwcvdate[.source == "today"],
        nextwcvdate[.source == "prior"]
      ),
      next_wcv_entry_date = coalesce(
        next_wcv_entry_date[.source == "today"],
        next_wcv_entry_date[.source == "prior"]
      ),
      nextwcvtype = coalesce(
        nextwcvtype[.source == "today"],
        nextwcvtype[.source == "prior"]
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.source == "today") %>%
    dplyr::select(-.source)

  # Final cleanup
  comparison_df <- comparison_df %>%
    dplyr::mutate(
      language = "",
      site = NA,
      form_1_complete = 2
    )

  return(list(
    prior_review = comparison_df,
    prior_combined = prior_combined
  ))
}

