# simulate data
N_persub <- 100
N <- 100
p <- 3
p_noise <- 7

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

dat <- simulate_HMM_data(
  seed_num = 20,
  p_noise = p_noise,
  N = N,
  N_persub = N_persub,
  parameters_setting = parameters_setting
)


y_long <- rep(NA, N * N_persub)
X_array_with_noise_long <- matrix(NA,
                                  nrow = N * N_persub, ncol = p + p_noise + 1)
for (i in 1:N) {
  index_replace <- 1:N_persub + (i - 1) * N_persub
  # print(index_replace)
  y_long[index_replace] <- dat$y_mat[i, ]
  X_array_with_noise_long[index_replace, ] <- dat$X_array[, , i]
}

test_that("At least no error", {
  emiss_mat_start <- matrix(NA, nrow = 2, ncol = p + p_noise + 1)
  delta_start <- rep(NA, 2)
  
  trans_mat_start <- matrix(NA, nrow = 2, ncol = 2)
  trans_mat_start[1, ] <- c(0.6, 0.40)
  trans_mat_start[2, ] <- c(0.30, 0.70)
  emiss_mat_start[1, ] <- 0.1
  emiss_mat_start[2, ] <- 0.1
  delta_start[1] <- 0.6
  delta_start[2] <- 1 - delta_start[1]
  
  rHMM_get <- rHMM(
    delta = delta_start,
    Y_mat = dat$y_mat,
    A = trans_mat_start,
    B = emiss_mat_start,
    X_cube = dat$X_array,
    family = "P",
    trace = 0,
    N_iter = 20,
    eps = 1e-04
  )
  
  # no error so that it gets here
  expect_equal(1, 1)
})
