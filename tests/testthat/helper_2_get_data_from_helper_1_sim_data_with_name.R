#source("sim_data.R")
## 2 states, 3 covariates, Poisson, 100 subjects
## 10 observations for each subject

parameters_setting <- list()
parameters_setting$emis_mat <- matrix(NA, nrow = 2, ncol = 4)
parameters_setting$emis_mat[1, 1] <- 0.1
parameters_setting$emis_mat[1, 2] <- 0.5
parameters_setting$emis_mat[1, 3] <- -0.75
parameters_setting$emis_mat[1, 4] <- 0.75
parameters_setting$emis_mat[2, 1] <- -0.1
parameters_setting$emis_mat[2, 2] <- -0.5
parameters_setting$emis_mat[2, 3] <- 0.75
parameters_setting$emis_mat[2, 4] <- 1

parameters_setting$trans_mat <- matrix(NA, nrow = 2, ncol = 2)
parameters_setting$trans_mat[1, 1] <- 0.65
parameters_setting$trans_mat[1, 2] <- 0.35
parameters_setting$trans_mat[2, 1] <- 0.2
parameters_setting$trans_mat[2, 2] <- 0.8

parameters_setting$init_vec <- c(0.65, 0.35)

parameters_setting$b <- 0.6

dat_mHMM_with_name <- simulate_mHMM_data_with_name(
  seed_num = 1,
  p_noise = 7,
  N = 1000,
  N_persub = 10,
  parameters_setting = parameters_setting
)
