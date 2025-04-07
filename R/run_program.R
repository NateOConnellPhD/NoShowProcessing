#' Run the full daily no-show trial data pipeline
#'
#' This function performs the full daily data processing workflow for the no-show trial.
#' It reads in the most recent master files, processes new data, identifies eligibles
#' and ineligibles, merges with prior data, performs cleaning steps, and saves all
#' relevant output files.
#'
#' Steps performed:
#' \itemize{
#'   \item Loads current and previous master datasets
#'   \item Imports and processes today's raw data
#'   \item Extracts no-show data from 7 days ago
#'   \item Identifies ineligible patients (e.g., language, reschedule, no phone)
#'   \item Identifies eligible patients
#'   \item Merges today's data with prior reviewed data
#'   \item Applies post-cleaning to prepare full import list
#'   \item Saves updated outputs to disk
#' }
#'
#' @return No return value. Saves processed data to disk.
#' @export
#' 
run_program = function(){
  
  master = readRDS("processed_data/master.rda")
  master_prev = readRDS("processed_data/20250404_master.rda")
  
  ##### Set up todays list  ############
  master_today = list()
  
  ###### Import data ##########
  df = import_df()
  
  ###### Process Data (fix phone numbers, label sites, and remove duplicates) #########
  df_proc = process_df(df)
  master_today$df_proc = df_proc
  
  ###### Get 7 day data #########
  df_7day = get_7_day(df_proc)
  master_today$df_7day = df_7day
  
  ###### Get ineligibles ########
  df_inels = get_inels(df_7day)
  master_today$inel = df_inels
  
  ######### Get Eligible Patients to import ###########
  df_els = get_eligible(df_7day)
  master_today$eligibles = df_els
  
  ###### Merge and  Clean ###############
  df_merge = merge_and_clean(master_today, master_prev)
  master_today$prior_combine = df_merge$prior_combined
  master_today$prior_review = df_merge$prior_review
  
  ########## Post_cleaning ##############
  new_master = new_master(master_today)
  
  ######### Save Files #############
  save_files(master_today, master)
}