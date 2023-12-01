library("dplyr")
get_data <- dat_mHMM_with_name

form_test <- "response ~ column1 + column2 + column3 + column4 + column5 + column6 + column7 + column8 + column9 + column10 + (1|ID)"
get_data_list_array_from_dataset <- form_model_array(form_test, get_data, "ID", "time")

id_vec <- unique(get_data$ID)

X_cube <- array(NA, dim=c(10,
                          11,
                          length(id_vec)
)
)

Z_cube <- array(NA, dim=c(10,
                          1,
                          length(id_vec)
)
)

y_mat <- matrix(NA,
                nrow=length(id_vec),
                ncol=10)

for(i in 1:length(id_vec)){
  each_sub_df <- get_data %>%
    filter(ID==id_vec[i])
  matrix_each_sub <- model.matrix(response ~ column1 + column2 + column3 + column4 + column5 + column6 + column7 + column8 + column9 + column10 ,
                                  data=each_sub_df)
  X_cube[,,i] <- matrix_each_sub
  y_mat[i,] <- each_sub_df$response
  Z_cube[,1,i] <- 1
}

test_that("X matrices should be all the same", {
  expect_equal(sum(X_cube!=get_data_list_array_from_dataset$X_cube), 0)
})

test_that("Z matrices should be all the same", {
  expect_equal(sum(Z_cube!=get_data_list_array_from_dataset$Z_cube), 0)
})

test_that("Y matrix should be the same", {
  expect_equal(sum(y_mat!=get_data_list_array_from_dataset$y_mat), 0)
})


