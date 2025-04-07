#' Get Ineligible Patients for Outreach
#'
#' Identifies subsets of patients who are ineligible for outreach based on:
#' 1) having a rescheduled well-child visit (WCV),
#' 2) preferring a language other than English or Spanish,
#' or 3) not having a valid phone number on file.
#'
#' @param df A data frame containing patient-level data, including columns for:
#'   - `completed_wcv_date`
#'   - `next_wcv_date`
#'   - `completed_wcv_type`
#'   - `next_wcv_type`
#'   - `language`
#'   - `home_phone`
#'   - `cell_phone`
#'
#' @return A named list with three data frames:
#' \describe{
#'   \item{`resched`}{Patients who already have another WCV scheduled (based on completed visit info).}
#'   \item{`lang`}{Patients whose preferred language is not English (`"en"`) or Spanish (`"es"`).}
#'   \item{`nophone`}{Patients missing a valid home or cell phone number.}
#' }
#'
#' @examples
#' \dontrun{
#' results <- get_inels(patient_data)
#' results$resched   # View patients with rescheduled WCVs
#' results$lang      # View patients with unsupported language
#' results$nophone   # View patients with no valid phone number
#' }
#'
#' @export


get_inels = function(df){
  
  today <- Sys.Date()
  
  # Create a date string in CCYYMMDD format for use in filenames
  dateformat <- format(today, "%Y%m%d")
  
  #### SECTION 1: Patients with no-shows that already have another WCV scheduled ####
  # Update next_wcv_date and next_wcv_type where appropriate,
  # then keep only observations with a non-missing next_wcv_date.
  df_inelig_resched <- df %>%
    mutate(
      nextwcvdate = if_else(
        condition = !is.na(completedwcvdate),
        true = completedwcvdate,
        false = nextwcvdate
      ),
      nextwcvtype = if_else(
        condition = completedwcvtype != "",
        true = completedwcvtype,
        false = nextwcvtype
      )
    ) %>%
    filter(!is.na(nextwcvdate))
  
  
  #### SECTION 2: Patients whose preferred language is not English or Spanish ####
  df_inelig_lang <- df %>%
    filter(!(language %in% c("en", "es")))
  
  
  #### SECTION 3: Patients without a phone number on file ####
  df_inelig_nophone <- df %>%
    filter(
      home_phone == "" |
        is.na(home_phone) | 
        home_phone == "000-000-0000" 
    )
  
  
  return(list(`resched` = df_inelig_resched,
              `lang` = df_inelig_lang,
              `nophone` = df_inelig_nophone))
}
