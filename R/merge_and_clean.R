
  #' Merge and Clean Daily and Prior Data
  #'
  #' This function merges the current day's processed dataset with prior days' eligible and reviewed data,
  #' filters and cleans the merged dataset, reconciles discrepancies in key WCV fields, and outputs a cleaned
  #' version ready for review or export.
  #'
  #' @param master_today A named list containing today's datasets. Must include df_proc, the day's full processed data.
  #' @param master_prev A named list containing prior datasets. Must include eligibles and prior_review from the previous day (typically Friday).
  #'
  #' @return A named list with two data frames:
  #' \describe{
  #'   \item{prior_review}{Cleaned and reconciled data from today, suitable for REDCap re-upload.}
  #'   \item{prior_combined}{Raw merged prior data from eligibles and prior_review.}
  #' }
  #'
  #' @details
  #' The function:
  #' \itemize{
  #'   \item Filters today's data to English/Spanish speakers and removes recent no-shows (<7 days ago).
  #'   \item Merges today's data with prior data to retrieve redcap_repeat_instance and other WCV fields.
  #'   \item Flags discrepancies in WCV fields and reconciles values based on presence in today's or prior data.
  #'   \item Drops unmatched records and prepares a cleaned dataset with appropriate REDCap formatting.
  #' }
  #' Household-level clustering is not directly addressed but could impact longitudinal analyses.
  #'
  #' @import dplyr
  #' @import lubridate
  #' @export

  ### This merges data with the previous days data and cleans it
  merge_and_clean = function(master_today, master_prev, date){

    # Determine today's date and day of the week
    today_date <- if (is.null(date)) Sys.Date() else as.Date(date, format = "%Y%m%d")
    date_string <- format(today_date, "%Y%m%d")

    day_of_week <- wday(today_date, week_start = 1) # Monday = 1, Sunday = 7

    # Get previous day(s)
    friday_date <- today_date - 3
    yesterday_date <- today_date - 1

    # Use current day's full dataset
    df_today_all <- master_today$df_proc

    # Filter: drop if language is not English or Spanish
    df_today_all <- df_today_all %>%
      filter(language %in% c("en", "es"))

    # Drop newly eligible and later visits (no-shows must be >= 7 days ago)
    df_today_all <- df_today_all %>%
      filter(datenoshow < today_date - 7)

    #get prior combined data
    prior_combined <- bind_rows(
      master_prev$eligibles,         # from Friday
      master_prev$prior_review        # from Friday
    )

    #Merge todays dataset with prior combined to pull in redcap_repeat_instance
    #names(df_today_all)
    names(prior_combined)[c(5,12:20,27:28,30:31)] = c("cellphone", "datenoshow","timenoshow_24hr", "timenoshow","deptexternalname", "visittypename",
                                                      "visitstatus","visitexternalname", "ageinyears", "ageinmonths","nextwcvdate","nextwcvtype",
                                                      "prior2yrwcvnoshow","prior2yrvisitnoshow")

    df_merged <- df_today_all %>%
      left_join(
        prior_combined %>%
          select(home_phone, pat_first_name, datenoshow, timenoshow, redcap_repeat_instance),
        by = c("home_phone", "pat_first_name", "datenoshow", "timenoshow")
      )

    # Drop unmatched observations (only keep those found in both datasets)
    df_merged <- df_merged %>%
      filter(!is.na(redcap_repeat_instance))

    # For change tracking: append prior_combined data to the merged set
    comparison_df <- bind_rows(
      df_merged %>% mutate(.source = "today"),
      prior_combined %>% mutate(.source = "prior")
    )

    # Sort and identify discrepancies in WCV fields
    comparison_df <- comparison_df %>%
      arrange(home_phone, pat_first_name, datenoshow, desc(.source)) %>%
      group_by(home_phone, pat_first_name, datenoshow) %>%
      mutate(
        diff_next_wcv_date = n_distinct(nextwcvdate) > 1,
        diff_next_wcv_entry_date = n_distinct(next_wcv_entry_date) > 1,
        diff_next_wcv_type = n_distinct(nextwcvtype) > 1
      ) %>%
      ungroup()


    # Retain latest values where prior info is missing, and vice versa
    comparison_df <- comparison_df %>%
      group_by(home_phone, pat_first_name, timenoshow_24hr) %>%
      mutate(
        nextwcvdate = {
          v1 <- nextwcvdate[.source == "today"]
          v2 <- nextwcvdate[.source == "prior"]
          if (length(v1) == 0) v1 <- NA
          if (length(v2) == 0) v2 <- NA
          coalesce(v1, v2)
        },
        next_wcv_entry_date = {
          v1 <- next_wcv_entry_date[.source == "today"]
          v2 <- next_wcv_entry_date[.source == "prior"]
          if (length(v1) == 0) v1 <- NA
          if (length(v2) == 0) v2 <- NA
          coalesce(v1, v2)
        },
        nextwcvtype = {
          v1 <- nextwcvtype[.source == "today"]
          v2 <- nextwcvtype[.source == "prior"]
          if (length(v1) == 0) v1 <- ""
          if (length(v2) == 0) v2 <- ""
          coalesce(v1, v2)
        }
      ) %>%
      ungroup() %>%
      filter(.source == "today") %>%
      select(-.source)


    # Post-cleaning: clear out randomized fields if needed
    comparison_df <- comparison_df %>%
      mutate(
        language = "",
        site = NA,
        form_1_complete = 2  # mark as complete for REDCap
      )

    # Save to today's processed prior-reviewed dataset
    return(list(prior_review= comparison_df,
                prior_combined = prior_combined))
  }
