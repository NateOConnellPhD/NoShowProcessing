#' Post-Cleaning for Full Import List Preparation
#'
#' This function performs post-cleaning steps on no-show trial data by reconciling today's new eligibles with
#' a historical full import list. It addresses inconsistencies in household and patient-level data,
#' updates repeat instances, and ensures REDCap compatibility before creating a new full import list.
#'
#' @param master_today A list containing today's cleaned data, with at least a `master_today$eligibles` data frame.
#' @param master A list containing historical data, including `master$full`, the full import list from previous days.
#'
#' @return A data frame combining the cleaned historical data and today's cleaned eligibles, with updates to
#'         `redcap_repeat_instance`, phone numbers, and other REDCap-ready fields.
#'
#' @details
#' This function:
#' \itemize{
#'   \item Appends today's eligibles to the historical full import list
#'   \item Identifies and harmonizes repeat patient and household records
#'   \item Updates `redcap_repeat_instance` for today's entries where applicable
#'   \item Clears `language` and `site` for non-primary household records when needed
#'   \item Returns the finalized full import list for export and future reuse
#' }
#'
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' updated_list <- post_clean(master_today, master)
#' }

post_clean <- function(master_today, master) {
  
  
  # Set today's date
  today_date <- Sys.Date()
  
  # Make column names consistent
  names(master$full) <- names(master_today$eligibles)
  
  # Combine today's new eligibles with full import list
  data_new <- bind_rows(
    master_today$eligibles %>% mutate(today = TRUE),
    master$full %>% mutate(today = FALSE)
  )
  
  ### --- Household/Patient-Level Consistency Checks --- ###
  
  # Household-level: create ID and repeat indicators
  data_new <- data_new %>%
    arrange(home_phone, datenoshow, timenoshow_24hr) %>%
    group_by(home_phone) %>%
    mutate(count_household = cur_group_id(),
           repeat_household = n()) %>%
    ungroup()
  
  
  # Patient-level: track duplicates and row order
  data_new <- data_new %>%
    arrange(ah_mrn, datenoshow) %>%
    group_by(ah_mrn) %>%
    mutate(
      repeat_pat = n(),
      repeat_pat_n = if_else(repeat_pat > 1, row_number(), 0)
    ) %>%
    ungroup()
  
  
  ### --- Patient-Level Differences in Phone/Site --- ###
  data_new <- data_new %>%
    group_by(ah_mrn) %>%
    mutate(
      pat_diffphone = if_else(repeat_pat > 1 & home_phone != first(home_phone), row_number(), NA_integer_),
      pat_diffsite = if_else(repeat_pat > 1 & site != first(site), row_number(), NA_integer_),
      home_phone = if_else(!is.na(pat_diffphone), first(home_phone), home_phone)
    ) %>%
    ungroup() %>%
    mutate(pat_diffsite = if_else(is.na(site), NA_integer_, pat_diffsite))
  
  ### --- Household-Level Differences --- ###
  data_new <- data_new %>%
    arrange(count_household, datenoshow, timenoshow_24hr) %>%
    group_by(count_household) %>%
    mutate(
      repeat_household_n = if_else(repeat_household > 1, row_number(), 0),
      count_hh_diffphone = if_else(home_phone != first(home_phone) & repeat_household_n > 0, row_number(), NA_integer_),
      count_hh_diffsite = if_else(site != first(site) & repeat_household_n > 0, row_number(), NA_integer_)
    ) %>%
    ungroup() %>%
    mutate(count_hh_diffsite = if_else(is.na(site), NA_integer_, count_hh_diffsite))
  
  ### --- Update REDCap Repeat Instance --- ###
  data_new <- data_new %>%
    mutate(
      redcap_repeat_instance = if_else(
        today &
          redcap_repeat_instance != repeat_household_n &
          repeat_household_n > 0 &
          datenoshow == today_date - 7,
        repeat_household_n,
        redcap_repeat_instance
      )
    )
  
  ### --- Final Cleanup for Import --- ###
  data_excl <- data_new %>%
    filter(!today) %>%
    group_by(home_phone) %>%
    mutate(lowest = min(redcap_repeat_instance, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      language = if_else(redcap_repeat_instance > 1 & lowest > 1, "", language),
      site = if_else(redcap_repeat_instance > 1 & lowest > 1, NA, site)
    ) %>%
    select(-today, -count_household, -repeat_household, -repeat_pat,
           -repeat_pat_n, -pat_diffphone, -pat_diffsite,
           -repeat_household_n, -count_hh_diffphone, -count_hh_diffsite, -lowest)
  
  
  
  full_import_list <- bind_rows(
    data_excl,
    master_today$eligibles
  )
  
  return(full_import_list)
}
