N_persub <- 10000

test_that("No underflow/overflow", {
  forward_C <- forward(
    delta = parameters_setting$init_vec,
    Y = dat$y_mat[1, ],
    A = parameters_setting$trans_mat,
    B = parameters_setting$emis_mat,
    X = dat$X_array[, 1:4, 1],
    family = "P"
  )
  
  expect_equal(any(
    c( is.null(forward_C), is.na(forward_C) )
  ),
  FALSE)
})

test_that("Backward times forward should be getting the same llh-likelihood as the llh function", {
  forward_C <- forward(
    delta = parameters_setting$init_vec,
    Y = dat$y_mat[1, ],
    A = parameters_setting$trans_mat,
    B = parameters_setting$emis_mat,
    X = dat$X_array[, 1:4, 1],
    family = "P"
  )
  llh_C <- compute_loglikelihood(
    delta = parameters_setting$init_vec,
    Y = dat$y_mat[1, ],
    A = parameters_setting$trans_mat,
    B = parameters_setting$emis_mat,
    X = dat$X_array[, 1:4, 1],
    family = "P"
  )
  
  max_forward_C <- max(forward_C[, N_persub])
  llh_forward_C <- max_forward_C +
    log(sum(exp(forward_C[, N_persub] - max_forward_C)))
  expect_equal(llh_forward_C, llh_C)
})
