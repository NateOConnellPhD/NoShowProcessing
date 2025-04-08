#' Get Eligible Patients for Outreach
#'
#' Filters and formats a dataset of potential outreach patients to create a REDCap-ready import file.
#' This function removes ineligible entries, adjusts the repeat instance ID for households with
#' multiple children, and performs final cleanup for import.
#'
#' @param df A data frame containing patient-level data, including:
#'   - `completed_wcv_date`, `next_wcv_date`
#'   - `completed_wcv_type`, `next_wcv_type`
#'   - `language`, `home_phone`, `cell_phone`
#'   - Other optional duplicate tracking variables
#' @param date Optional reference date in "YYYYMMDD" format. If not supplied, uses today's date.
#'
#' @return A data frame of patients eligible for import into REDCap, with:
#' \describe{
#'   \item{`redcap_repeat_instance`}{Numeric indicator of household instance for REDCap repeating form.}
#'   \item{`form_1_complete`}{Flag indicating form is complete.}
#'   \item{All original variables except those dropped during processing.}
#' }
#'
#' @details The following steps are performed:
#' \enumerate{
#'   \item Updates `next_wcv_date` and `next_wcv_type` if `completed_wcv_date/type` exist.
#'   \item Filters out patients with:
#'     \itemize{
#'       \item a scheduled/rescheduled visit (`next_wcv_date` not missing),
#'       \item unsupported language (not "en" or "es"),
#'       \item invalid or missing phone numbers.
#'     }
#'   \item Drops unnecessary variables and assigns `redcap_repeat_instance = 1`.
#'   \item Identifies households with multiple patients and assigns increasing `redcap_repeat_instance`.
#'   \item Drops temporary or unused duplicate-tracking variables.
#'   \item Adds a completion flag (`form_1_complete = 1`).
#'   \item Excludes specific test or advisor phone numbers.
#' }
#'
#' @examples
#' \dontrun{
#' eligible <- get_eligible(patient_data)
#' head(eligible)
#' table(eligible$redcap_repeat_instance)
#' }
#'
#' @export
get_eligible <- function(df, date = NULL) {
  
  # Use specified date or default to today
  today <- if (is.null(date)) Sys.Date() else as.Date(date, format = "%Y%m%d")
  
  #### PREPARE ELIGIBLE NEW PATIENTS FOR IMPORT ####
  df <- df %>%
    mutate(
      nextwcvdate = if_else(!is.na(completedwcvdate), completedwcvdate, nextwcvdate),
      nextwcvtype = if_else(completedwcvtype != "", completedwcvtype, nextwcvtype)
    ) 
  
  import_df <- df %>%
    filter(is.na(nextwcvdate)) %>%
    filter(language %in% c("en", "es")) %>%
    filter(!(home_phone == "" | is.na(home_phone) | home_phone == "000-000-0000")) %>%
    select(-completedwcvdate, -completedwcvtype)
  
  # Assign redcap_repeat_instance
  import_df <- import_df %>%
    mutate(redcap_repeat_instance = 1) %>%
    relocate(redcap_repeat_instance, .before = cellphone)
  
  # Household duplicates
  import_df <- import_df %>%
    group_by(home_phone) %>%
    mutate(
      dup_house_tag = if_else(n() > 1, 1L, 0L),
      dup_house_id  = row_number()
    ) %>%
    ungroup() %>%
    mutate(
      dup_house_id = if_else(dup_house_tag == 0, 0L, dup_house_id),
      redcap_repeat_instance = if_else(dup_house_id > 1, dup_house_id, redcap_repeat_instance)
    )
  
  # Drop temp variables
  import_df <- import_df %>%
    select(-one_of("dup_3wk_house_tag", "dup_3wk_house_id", "dup_pat_tag",
                   "quick_dup_pat", "noshowdiff", "dup_house_tag", "dup_house_id"))
  
  # Mark form as complete
  import_df <- import_df %>%
    mutate(form_1_complete = 1)
  
  # Drop advisor children
  import_df <- import_df %>%
    filter(!(home_phone %in% c("336-989-6601", "336-844-0038", 
                               "704-839-1413", "336-972-7994")))
  
  return(import_df)
}

