#' Gets eligible patients based on 7 day visit criteria
#'
#' Processes data further and reduces dataset to patients eligibility based on their appoitnment having occured 7 days prior. 
#' @param df the name of the dataset produces from "process_data()" function
#' @return Dataframe containing only patients with missed visits from exactly 7 days ago
#' @examples 
#' get_7_day(df)
#' @export

get_7_day = function(df){
  df <- df %>%
    group_by(home_phone) %>%
    mutate(dup_3wk_house_tag = if_else(n() > 1, 1L, 0L),
           dup_3wk_house_id = row_number()) %>%
    ungroup() %>%
    mutate(dup_3wk_house_id = if_else(dup_3wk_house_tag == 0, 0L, dup_3wk_house_id))
  
  # --- Duplicate patients with close succession no-shows ---
  df <- df %>%
    # Create a new variable "patient_id" that is wakeone_mrn unless it's NA, then use pat_first_name
    mutate(patient_id = coalesce(as.character(wakeone_mrn), pat_first_name)) %>%
    # Group by home_phone and the new patient_id
    group_by(home_phone, patient_id) %>%
    arrange(datenoshow, timenoshow) %>%  # sort within group by no-show dates and times
    mutate(
      dup_pat_tag = if_else(n() > 1, 1L, 0L),
      quick_dup_pat = row_number()          # sequential number within the group
    ) %>%
    ungroup() %>%
    mutate(
      quick_dup_pat = if_else(dup_pat_tag == 0, 0L, quick_dup_pat)
    )
  
  # --- Calculate the number of days difference between the first two no-show dates ---
  df <- df %>%
    group_by(home_phone, patient_id) %>%
    arrange(datenoshow, timenoshow) %>%
    mutate(noshowdiff = if_else(n() >= 2, as.numeric(nth(datenoshow, 2) - first(datenoshow)), NA_real_)) %>%
    ungroup()
  
  # --- For groups where the gap between the first and second no-shows is > 7 days, reset quick_dup_pat to 0 ---
  df <- df %>%
    mutate(quick_dup_pat = if_else(!is.na(noshowdiff) & noshowdiff > 7, 0L, quick_dup_pat))
  
  # --- Within each duplicate patient group with quick_dup_pat == 1 and noshowdiff < 8, 
  # update next_wcv_date to be the second no-show date
  df <- df %>%
    group_by(home_phone, pat_first_name) %>%
    mutate(nextwcvdate = if_else(quick_dup_pat == 1 & !is.na(noshowdiff) & noshowdiff < 8,
                                   nth(datenoshow, 2),
                                   nextwcvdate)) %>%
    ungroup()
  
  # --- Revert sorting order to home_phone, date_no_show, time_no_show ---
  df <- df %>% arrange(home_phone, datenoshow, timenoshow) 
  
  # --- Subset the data to those records where date_no_show is exactly 7 days before "today" ---
  # You can replace Sys.Date() with a specific date if needed
  today <- Sys.Date()
  df <- df %>% filter(datenoshow == (today - 7))
  
  df = df %>% select(!patient_id)
  
  return(df)
}
