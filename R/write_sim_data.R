#' Write simulation data
#'
#' @return "Done" message

write_sim_data <- function(task_id, history_augmented, history_2, observed_a_augmented, observed_b_augmented) {
  # capture histories
  # data structure: rows = individuals; columns = capture occasions

  # .csv format
  write.table(history_augmented,
              file = paste0("./", task_id, "_h_aug.csv"),
              col.names = FALSE, sep = ",", row.names = FALSE)

  # capture histories with 2s for recaptures
  # data structure: rows = individuals; columns = capture occasions

  # .csv format
  write.table(history_2,
              file = paste0("./", task_id, "_h_2.csv"),
              col.names = FALSE, sep = ",", row.names = FALSE)

  # observed a histories
  # data structure: rows = individuals; columns = capture occasions

  # .csv format
  write.table(observed_a_augmented,
              file = paste0("./", task_id, "_obs_a_aug.csv"),
              col.names = FALSE, sep = ",", row.names = FALSE)

  # observed b histories
  # data structure: rows = individuals; columns = capture occasions

  # .csv format
  write.table(observed_b_augmented,
              file = paste0("./", task_id, "_obs_b_aug.csv"),
              col.names = FALSE, sep = ",", row.names = FALSE)

  return(paste0("Done writing output data: task #", task_id, " h_aug h_2 obs_a_aug obs_b_aug"))
}
