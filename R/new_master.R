new_master <- function(master_today, master) {

  master_file <- "processed_data/master.rda"

  if (file.exists(master_file)) {

    # Merge eligibles and full (data frames)
    eligibles <- bind_rows(master$eligibles, master_today$eligibles)
    full <- master$full

    # Merge inel components individually
    inel_keys <- union(names(master$inel), names(master_today$inel))
    inel <- setNames(vector("list", length(inel_keys)), inel_keys)
    for (key in inel_keys) {
      df_old <- master$inel[[key]]
      df_new <- master_today$inel[[key]]
      if (!is.null(df_old) && !is.null(df_new)) {
        inel[[key]] <- bind_rows(df_old, df_new)
      } else if (!is.null(df_old)) {
        inel[[key]] <- df_old
      } else if (!is.null(df_new)) {
        inel[[key]] <- df_new
      }
    }

  } else {
    eligibles <- master_today$eligibles
    inel <- master_today$inel
    full <- master$full
  }

  list(eligibles = eligibles, inel = inel, full = full)
}



# new_master <- function(master_today, master, date) {
#   dateformat <- date
#   master_file <- "processed_data/master.rda"
#   new_master_path <- paste0("processed_data/", dateformat, "_master.rda")
#
#   if (file.exists(master_file)) {
#
#
#     # Merge eligibles and full (data frames)
#     eligibles <- bind_rows(old_master$eligibles, master_today$eligibles)
#     full <- post_clean(master_today, old_master)
#
#     # Merge inel components individually
#     inel_keys <- union(names(old_master$inel), names(master_today$inel))
#     inel <- setNames(vector("list", length(inel_keys)), inel_keys)
#     for (key in inel_keys) {
#       df_old <- old_master$inel[[key]]
#       df_new <- master_today$inel[[key]]
#       if (!is.null(df_old) && !is.null(df_new)) {
#         inel[[key]] <- bind_rows(df_old, df_new)
#       } else if (!is.null(df_old)) {
#         inel[[key]] <- df_old
#       } else if (!is.null(df_new)) {
#         inel[[key]] <- df_new
#       }
#     }
#
#   } else {
#     eligibles <- master_today$eligibles
#     inel <- master_today$inel
#     full <- post_clean(master_today, master_today)
#   }
#
#   list(eligibles = eligibles, inel = inel, full = full)
# }
#
#
#
#
#
#
#
#
