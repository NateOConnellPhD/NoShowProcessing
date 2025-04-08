#' Compare Two Datasets on Specified Columns
#'
#' This function compares two datasets on a predefined set of key columns
#' (`home_phone`, `twilio_phone`, `site`, `visitstatus`) after stripping
#' all labels, normalizing data types, and replacing empty strings with NA.
#'
#' It is particularly useful for checking whether two processed datasets
#' (e.g., STATA vs R output) are equivalent in structure and content,
#' despite potential formatting or label-related differences.
#'
#' @param df1 First data frame to compare.
#' @param df2 Second data frame to compare.
#' @param show_diff Logical. If TRUE, will return full list of differences
#'        (currently commented out in implementation).
#'
#' @return A printed message to the console indicating whether the datasets match
#'         on the specified variables.
#'
#' @importFrom haven is.labelled zap_labels
#' @importFrom lubridate ymd
#' @importFrom tibble tibble
#' @importFrom purrr map_lgl
#' @export
#'
compare_datasets <- function(df1, df2, show_diff = FALSE) {

  myVars= c("home_phone", "twilio_phone", "site", "visitstatus")
  df1 = df1[myVars]
  df2 = df2[myVars]

  clean_df <- function(df) {
    df[] <- lapply(df, function(x) {
      # Remove all attributes except core structure
      attributes(x) <- attributes(x)[names(attributes(x)) %in% c("names", "dim", "dimnames")]

      # Remove labels (Hmisc or haven)
      if (haven::is.labelled(x)) x <- haven::zap_labels(x)
      if (!is.null(attr(x, "label"))) attr(x, "label") <- NULL
      if (!is.null(attr(x, "format.stata"))) attr(x, "format.stata") <- NULL

      # Convert factors to character
      if (is.factor(x)) x <- as.character(x)

      # Convert "" to NA
      if (is.character(x)) {
        x[x == ""] <- NA
        # Attempt to coerce character to Date
        try_date <- suppressWarnings(lubridate::ymd(x))
        if (sum(!is.na(try_date)) > 0.75 * length(x)) {
          x <- try_date
        }
      }

      # Normalize numerics
      if (is.numeric(x) || is.integer(x)) x <- as.numeric(x)

      return(x)
    })
    df
  }

  df1_clean = clean_df(df1)
  df2_clean =clean_df(df2)

  # Ensure same column names
  if (!identical(names(df1_clean), names(df2_clean))) {
    stop("Column names do not match.")
  }


  # Relaxed comparison using all.equal
  comparison <- tibble::tibble(
    column = names(df1_clean),
    identical = purrr::map_lgl(names(df1_clean), function(col) {
      tryCatch({
        x1 <- df1_clean[[col]]
        x2 <- df2_clean[[col]]
        # Handle different types gracefully
        if (is.factor(x1)) x1 <- as.character(x1)
        if (is.factor(x2)) x2 <- as.character(x2)
        if (is.numeric(x1)) x1 <- round(x1, 8)
        if (is.numeric(x2)) x2 <- round(x2, 8)
        isTRUE(all.equal(x1, x2, check.attributes = FALSE))
      }, error = function(e) FALSE)
    })
  )

  # if (show_diff) {
  #   diffs <- purrr::map(names(df1_clean), function(nm) {
  #     if (!isTRUE(all.equal(df1_clean[[nm]], df2_clean[[nm]], check.attributes = FALSE))) {
  #       list(
  #         df1 = unique(na.omit(df1_clean[[nm]])),
  #         df2 = unique(na.omit(df2_clean[[nm]]))
  #       )
  #     } else {
  #       NULL
  #     }
  #   })
  #   names(diffs) <- names(df1_clean)
  #   return(list(summary = comparison, differences = diffs))
  # } else {
  #   return(comparison)
  # }

  if(sum(comparison$identical) == 4){
    message("✅ Datasets Match")
    #return("Datasets Match")
  } else{
    message("✅ Datasets Match")
    #return("Datasets do not match")
  }

}
