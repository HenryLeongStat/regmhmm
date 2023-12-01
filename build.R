library("devtools")
# usethis profile
## https://usethis.r-lib.org/articles/usethis-setup.html
options(
  usethis.description = list(
    "Authors@R" = utils::person(
      "Man Chong", "Leong",
      email = "mc.leong26@gmail.com",
      role = c("aut", "cre"),
      comment = c(ORCID = "0000-0003-3895-9527")
    )
  ),
  usethis.destdir = "/Users/mleong1/Documents/regmhmm",
  usethis.overwrite = TRUE
)

proj_sitrep()
git_sitrep()
# gitcreds::gitcreds_set()


# badge
## https://usethis.r-lib.org/articles/badge-accessibility.html


library("RcppArmadillo")
library("devtools")

# Add a dependency
use_package('glmnet')
use_package('glmnetUtils')
use_package('stats')
use_package('Rcpp')
use_package('MASS')
# use_package('igraph')

# use_git()
usethis::use_build_ignore(c("developping_R", "development", "test", "tests"))

# use_mit_license()

# Reload the package: CTRL-L or
devtools::load_all()
#roxygen2::roxygenize(package.dir = ".", roclets = NULL, load_code = NULL, clean = FALSE)

# (Re-)build NAMESPACE
devtools::document()

# Rcpp::compileAttributes()
Rcpp::compileAttributes(verbose=TRUE)
roxygen2::roxygenize(roclets="rd", clean=TRUE) 

codemetar::write_codemeta()
# codemetar::give_opinions('.')
# 
# usethis::use_citation()
#usethis::use_vignette("regmhmm", "regmhmm")

# (Re-)build NAMESPACE
devtools::document()

devtools::check()

devtools::build()
# devtools::install()

