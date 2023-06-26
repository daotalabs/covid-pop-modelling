library(tictoc)
library(nimble)
library(dplyr)
library(Rlab)

source("./write_sim_data.R")
source("./make_history.R")
source("./populate_obs_ab_by_row.R")
source("./build_nimble_model.R")

# R script for running a simulation study. Uses a parameter matrix
# created by generate_parameter_matrix.R. Takes the task id as a command
# line argument,which corresponds to a row of the parameter matrix.
# Outputs model_statistics_* text files with the summary statistics for the coda
# samples.


tic()

print("finished loading libraries")

args <- commandArgs(trailingOnly = TRUE)

task_id_str <- args[1]
task_id <- strtoi(args[1])

# number of repetitions for each simulation
reps <- args[2]

print(paste0("task id: ", task_id_str))

# parameter matrix from generate_parameter_matrix.R
params_matrix <- read.csv("./parameter_matrix.csv")

print("finished loading parameter matrix")

params <- params_matrix[task_id, ]

# generate data

# true population size
N <- as.integer(params["N"])

# augmented population size
M <- as.integer(params["M"])

# number of sampling occasions / strata
k <- as.integer(params["k"])

# true lab test presence probability
theta1 <- as.double(params["theta1"])

# true lab test capture probability
pa <- as.double(params["pa"])

# true hospital presence probability
theta2 <- as.double(params["theta2"])

# true hospital capture probability
pb <- as.double(params["pb"])

# for (rep in 1:reps) { # uncomment this if want repetitions
# setting seed
seed <- as.integer(1e7 * runif(1))
set.seed(seed)

print(paste0("seed: ", seed))

# simulate capture histories
sim_out <- make_history(N, M, k, theta1, pa, theta2, pb)

print(paste("sum(z_aug) =", sum(sim_out$z_aug == 1, na.rm = T)))
print(paste("n =", sim_out$n))

# save simulation data for debugging
write_sim_data(task_id_str,
               sim_out$h_aug, sim_out$h_2, sim_out$obs_a_aug, sim_out$obs_b_aug)

#-------------------- Call nimble from R -----------------------#
mcmc_out <- build_nimble_model(sim_out, chains = 1, iter = 25000, burnin = 10000)

if (is.null(mcmc_out)) {
  print("Error: model failed to work")
  next
}

# Get summary statistics
write.table(rbind(mcmc_out$summary, n = sim_out$n, n_2 = sim_out$n_2), file = paste0(task_id_str, "_model_statistics.csv"), sep = ",", col.names = F)

# Get posterior samples
write.table(mcmc_out$samples, file = paste0(task_id_str, "_model_post_samples.csv"), sep = ",", row.names = F)

print(paste0("Done task #", task_id))
toc()
