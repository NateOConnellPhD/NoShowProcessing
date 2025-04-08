#' Ensure Required Folders Exist
#'
#' This function checks for the existence of required folders used in the
#' `run_program()` workflow and creates them if they do not already exist.
#'
#' The required folders include:
#' \itemize{
#'   \item \code{processed_data/}: stores prior master RDS files used in cleaning and merging.
#'   \item \code{data/}: contains raw `.xlsx` files downloaded from SharePoint (e.g., `datarequest_1585_YYYYMMDD.xlsx`).
#'   \item \code{import/}: stores final `.csv` files created during the `run_program()` process for REDCap import.
#' }
#'
#' Optionally, a folder \code{stata_data/} can be included if `.dta` files are needed to build a master file via `create_stata_master()`.
#'
#' @param required_folders A character vector of folder paths to check/create.
#'   Defaults to \code{c("processed_data", "data", "import")}.
#'
#' @return Invisibly returns \code{TRUE} after ensuring all folders exist.
#' @export
#'
#' @examples
#' ensure_required_folders()
#' ensure_required_folders(c("processed_data", "data", "import", "stata_data"))
ensure_required_folders <- function(required_folders = c("processed_data", "data", "import")) {
  created_any <- FALSE

  for (folder in required_folders) {
    if (!dir.exists(folder)) {
      message("📁 Creating folder: ", folder)
      dir.create(folder, recursive = TRUE)
      created_any <- TRUE
    }
  }

  if (!created_any) {
    message("✅ All folders exist.")
  }

  invisible(TRUE)
}

