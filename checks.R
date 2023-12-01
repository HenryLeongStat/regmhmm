# checks

start.time <- Sys.time()
# R package CMD check
devtools::check()
end.time <- Sys.time()
time.taken <- difftime(end.time, start.time, units = "mins")
cat("It took ", time.taken, "mins to for devtools::check().")

# pkgcheck
library (pkgcheck)

# add workflow
# pkgcheck::use_github_action_pkgcheck()

# Sys.setenv ("GITHUB_TOKEN" = '')
# mydir <- file.path (tempdir (), "regmhmm_check")
# gert::git_clone ("https://github.com/HenryLeongStat/regmhmm", path = mydir)
start.time <- Sys.time()
pkgcheck_report <- pkgcheck (path=".",
                             use_cache = FALSE)
# pkgcheck_report <- pkgcheck (path=mydir)
end.time <- Sys.time()
time.taken <- difftime(end.time, start.time, units = "mins")
cat("It took ", time.taken, "mins to for pkgcheck (path=mydir).")

summary(pkgcheck_report)

# usethis check
spelling::spell_check_package()
# usethis::use_testthat()

# usethis test
# usethis::use_test("test_backward_algorithm")
# usethis::use_test("test_forward_algorithm")
# usethis::use_test("test_forward_backward_algorithm")
# usethis::use_test("test_compute_gamma")
# usethis::use_test("test_compute_xi")
# usethis::use_test("test_fit_HMM")
# usethis::use_test("test_fit_rHMM")
start.time <- Sys.time()
devtools::test()
end.time <- Sys.time()
time.taken <- difftime(end.time, start.time, units = "mins")
cat("It took ", time.taken, "mins to for devtools::test().")

# setup converage
library("covr")
# If run with the working directory within the package source.
start.time <- Sys.time()
# If run with the working directory within the package source.
cov <- package_coverage(".")
end.time <- Sys.time()
time.taken <- difftime(end.time, start.time, units = "mins")
cat("It took ", time.taken, "mins to for package_coverage('.').")

# shiny app for showing cov
# shine(cov)

# or a package in another directory
# cov <- package_coverage("/dir/lintr")

# view results as a data.frame
as.data.frame(cov)

# zero_coverage() shows only uncovered lines.
# If run within RStudio, `zero_coverage()` will open a marker pane with the
# uncovered lines.
zero_coverage(cov)

# vignette("how_it_works", package = "covr")

# exclude print functions
# package_coverage(function_exclusions = "print\\.")

# upload codecov report to github
# usethis::use_coverage( type = c("codecov"))
# usethis::use_github_action("test-coverage")

codecov(token = "1c0db22d-301f-4f53-973f-1b3fca83f6f6")

# Local Linux checks with Docker
library(rhub)
imgs <- local_check_linux_images()
imgs
pkg_path <- "."
result_local_check_vec <- vector(mode="list", length(imgs$`docker-image`))
for(i in 1:length(imgs$`docker-image`)){
  result_local_check <- local_check_linux(pkg_path, image = paste0("rhub/", imgs$`docker-image`[i]))
  result_local_check_vec[[i]] <- result_local_check
}

for(i in 1:length(imgs$`docker-image`)){
  print(result_local_check_vec[[i]])
}

cran_prep <- check_for_cran()
print(cran_prep$cran_summary())

check_windowns <- check_on_windows()

# usethis::use_github_action("test-coverage")
