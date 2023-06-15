library(tibble)
library(tidyr)
library(dplyr)
library(readr)

## Define simulation parameters
param_matrix <- crossing(
  N = 10000,
  M = 20000,
  k = 14,
  theta1 = c(0.05, 0.1, 0.15, 0.2, 0.25),
  pa = c(0.8, 0.85, 0.9, 0.95, 0.99),
  theta2 = c(0.01, 0.025, 0.05, 0.075, 0.1),
  pb = c(0.01, 0.05, 0.1, 0.15, 0.2)
) %>%
  rowid_to_column("sim_id")

write_csv(param_matrix, "./parameter_matrix.csv")


