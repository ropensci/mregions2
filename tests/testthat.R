if (identical(Sys.getenv("NOT_CRAN"), "true") ||
    identical(Sys.getenv("GITHUB_ACTIONS"), "true")) {

  library(testthat)
  library(mregions2)

  test_check("mregions2")
}
