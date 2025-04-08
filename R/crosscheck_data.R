#' Check All Master Components Against Reference Files
#'
#' Compares output from `master_today` and `new_master` against .dta files in the "data_check/" folder.
#'
#' @param master_today A list containing components generated during today's processing.
#' @param new_master A list containing the final processed full master object.
#' @param date Optional date string in "YYYYMMDD" format. Defaults to today.
#'
#' @return A message stating whether all datasets match and, if not, which components differ.
#' @export
crosscheck_data <- function(master_today, new_master, date = NULL) {
  require(haven)
  require(lubridate)

  # Load comparison function
  if (!exists("compare_datasets")) {
    stop("compare_datasets() must be defined.")
  }

  # Format dates
  today <- if (is.null(date)) Sys.Date() else as.Date(date, format = "%Y%m%d")
  date_string <- format(today, "%Y%m%d")

  # Get previous weekday for prior_combined
  prev_date <- today - ifelse(wday(today) == 2, 3, 1)
  prev_string <- format(prev_date, "%Y%m%d")

  # Load helper
  load_check_file <- function(name, date_prefix = date_string) {
    path <- file.path("data_check", paste0(date_prefix, "_", name, ".dta"))
    if (!file.exists(path)) stop(paste("Missing check file:", path))
    read_dta(path)
  }

  # List of all comparisons to run
  comparisons <- list(
    df_proc            = list(master_today$df_proc, load_check_file("all")),
    df_7day            = list(master_today$df_7day, load_check_file("7dayvisits")),
    inel_lang          = list(master_today$inel$lang, load_check_file("ineligibles_lang")),
    inel_resched       = list(master_today$inel$resched, load_check_file("ineligibles_resched")),
    inel_nophone       = list(master_today$inel$nophone, load_check_file("ineligibles_nophone")),
    eligibles          = list(master_today$eligibles, load_check_file("import_new")),
    prior_combined     = list(master_today$prior_combine, load_check_file("import_combined", prev_string)),
    prior_review       = list(master_today$prior_review, load_check_file("import_priorreviewed")),
    full_import_list   = list(new_master$full, read_dta("data_check/full_import_list.dta")),
    new_eligibles      = list(new_master$eligibles, read_dta("data_check/eligibles.dta")),
    new_inel_lang      = list(new_master$inel$lang, read_dta("data_check/ineligibles_lang.dta")),
    new_inel_resched   = list(new_master$inel$resched, read_dta("data_check/ineligibles_resched.dta")),
    new_inel_nophone   = list(new_master$inel$nophone, read_dta("data_check/ineligibles_nophone.dta"))
  )

  # Actually run comparisons
  mismatches <- purrr::keep(names(comparisons), function(name) {
    res <- compare_datasets(comparisons[[name]][[1]], comparisons[[name]][[2]])
    # If any column is not identical, we consider it a mismatch
    !all(res$identical)
  })

  # Report results
  if (length(mismatches) == 0) {
    message("✅ All datasets match.")
    return(TRUE)
  } else {
    warning("❌ Datasets do not match: ", paste(mismatches, collapse = ", "))
    return(mismatches)
  }
}
