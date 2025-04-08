#' Get Eligible Patients Based on 7-Day No-Show Criteria
#'
#' Processes and filters the dataset to identify patients who are eligible
#' for follow-up based on a missed visit that occurred exactly 7 days prior
#' to a specified date.
#'
#' @param df A data frame created by the `process_df()` function.
#' @param date A string date in "YYYYMMDD" format. If not supplied, defaults to today’s date.
#'
#' @return A data frame of eligible patients whose no-show occurred exactly 7 days ago.
#' @examples
#' get_7_day(df)  # Uses today's date
#' get_7_day(df, date = "20250405")  # Uses April 5, 2025 as reference
#' @export
get_7_day <- function(df, date = NULL) {
  # Resolve the reference date
  if (is.null(date)) {
    today <- Sys.Date()
  } else {
    today <- as.Date(date, format = "%Y%m%d")
    if (is.na(today)) stop("Date must be in 'YYYYMMDD' format.")
  }
  
  df <- df %>%
    group_by(home_phone) %>%
    mutate(
      dup_3wk_house_tag = if_else(n() > 1, 1L, 0L),
      dup_3wk_house_id = row_number(),
      dup_3wk_house_id = if_else(dup_3wk_house_tag == 0, 0L, dup_3wk_house_id)
    ) %>%
    ungroup()
  
  # --- Duplicate patients with close succession no-shows ---
  df <- df %>%
    mutate(patient_id = coalesce(as.character(wakeone_mrn), pat_first_name)) %>%
    group_by(home_phone, patient_id) %>%
    arrange(datenoshow, timenoshow) %>%
    mutate(
      dup_pat_tag = if_else(n() > 1, 1L, 0L),
      quick_dup_pat = if_else(dup_pat_tag == 0, 0L, row_number())
    ) %>%
    ungroup()
  
  # --- Calculate time gap between first and second no-shows ---
  df <- df %>%
    group_by(home_phone, patient_id) %>%
    arrange(datenoshow, timenoshow) %>%
    mutate(noshowdiff = if_else(n() >= 2, as.numeric(nth(datenoshow, 2) - first(datenoshow)), NA_real_)) %>%
    ungroup() %>%
    mutate(quick_dup_pat = if_else(!is.na(noshowdiff) & noshowdiff > 7, 0L, quick_dup_pat))
  
  # --- Update nextwcvdate for certain patients ---
  df <- df %>%
    group_by(home_phone, pat_first_name) %>%
    mutate(nextwcvdate = if_else(quick_dup_pat == 1 & !is.na(noshowdiff) & noshowdiff < 8,
                                 nth(datenoshow, 2),
                                 nextwcvdate)) %>%
    ungroup()
  
  # --- Final filtering ---
  df <- df %>%
    arrange(home_phone, datenoshow, timenoshow) %>%
    filter(datenoshow == (today - 7)) %>%
    select(-patient_id)
  
  return(df)
}

