#' Process dataframe 
#'
#' performs basic precursory cleaning of the dataframe from "import_df()". Removes duplicates, labels sites, adds redcap variables and re-orders variables
#' @param df the name of the dataframe stored from "import_df()"
#' @return cleaned/process version of the dataframe
#' @examples 
#' process_df(df)
#' @export
#' 

process_df = function(df){
  df = df %>%
    #  Rename 'preferred_language' to 'language' and recode values (English -> en, Spanish -> es)
    rename(language = preferred_language) %>%
    mutate(language = case_when(
      language == "English" ~ "en",
      language == "Spanish" ~ "es",
      TRUE ~ language
    )) %>%
    # For missing home_phone values (represented as an empty string), fill in with cell_phone value
    mutate(home_phone = ifelse(home_phone == "" | is.na(home_phone), cell_phone, home_phone)) %>%
    #  Generate twilio_phone as a copy of home_phone and place it before cell_phone
    mutate(twilio_phone = home_phone) %>%
    relocate(twilio_phone, .before = cell_phone) %>%
    # Create a numeric site variable based on department_name.
    # This will be used for randomization and alerts.
    mutate(site = case_when(
      department_name == "WFMG PC PPI FAM"       ~ 1,
      department_name == "WFMG FSLR PEDS"           ~ 2,
      department_name == "WFMG PC LAUREL CRK FAM"     ~ 3,
      department_name == "WFMG PC PEACE HAV FAM"      ~ 4,
      department_name == "WFMG CLEMMONS PEDS"         ~ 5,
      department_name == "WFMG PC LEWISVILLE FAM"     ~ 6,
      department_name == "WFMG PC REYNOLDA FAM"        ~ 7,
      department_name == "WFMG PC WESTBROOK FAM"        ~ 8,
      department_name == "WFMG WESTBROOK PEDS"          ~ 9,
      department_name == "WFMC PEDS DHP"                ~ 10,
      department_name == "WFMG KVILLE PEDS"             ~ 11,
      department_name == "WFMG KVILLE MAIN PEDS"        ~ 12,
      department_name == "WFMC WINSTON EAST PEDS"       ~ 13,
      department_name == "WFMG PC HIGHLAND FAM"         ~ 14,
      TRUE ~ NA_real_
    )) %>%
    # Optionally attach a label attribute to the site variable  
    { attr(.$site, "label") <- "Num DEPARTMENT_NAME"; . } %>%
    # Reorder site to be before 'pat_first_name'
    relocate(site, .before = pat_first_name) %>%
    # Rename the 12-hour format time_no_show variable to 'time_no_show'
    rename(time_no_show = time_no_show_12hr) %>% 
    # Generate the REDCap form variable for REDCap (all rows get "form_1")
    mutate(redcap_repeat_instrument = "form_1") %>%
    relocate(redcap_repeat_instrument, .before = cell_phone) %>%
    { attr(.$redcap_repeat_instrument, "label") <- "REDCap form name"; . } %>%
    #    Identify situations where an infant gets multiple appointments scheduled at the same time
    #    and drop the second (or later) appointment.
    #    We use the key variables: ah_mrn, date_no_show, and time_no_show.
    group_by(ah_mrn, date_no_show, time_no_show) %>%
    mutate(
      dup_record_tag = ifelse(n() > 1, 1, 0),  # Tag group if >1 obs (1 = duplicate, 0 = not)
      dup_record_id = row_number()               # Sequence number within group
    ) %>%
    ungroup() %>%
    # For groups with only one observation, set the duplicate record id to 0.
    mutate(dup_record_id = ifelse(dup_record_tag == 0, 0L, dup_record_id)) %>%
    # Drop observations that are duplicates (i.e. keep first scheduled appointment per group)
    filter(dup_record_id <= 1) %>%
    # Remove temporary duplicate tagging variables
    select(-dup_record_tag, -dup_record_id) %>%
    # sort by home_phone, date_no_show, and time_no_show.
    arrange(home_phone, date_no_show, time_no_show)
  
  target_names <- c(
    "home_phone", "twilio_phone", "redcap_repeat_instrument", "cellphone",
    "ah_mrn", "wakeone_mrn", "language", "department_name",
    "site", "pat_first_name", "datenoshow", "timenoshow_24hr",
    "timenoshow", "deptexternalname", "visittypename", "visitstatus",
    "visitexternalname", "ageinyears", "ageinmonths", "pri_insurance",
    "sec_insurance", "sex", "race", "ethnicity",
    "zipcode", "nextwcvdate", "nextwcvtype", "next_wcv_entry_date",
    "prior2yrwcvnoshow", "prior2yrvisitnoshow", "completedwcvdate", "completedwcvtype"
  )

  # Assign these names to your existing cleaned dataframe (e.g., df_proc)
  names(df) <- target_names
  return(df)
}
