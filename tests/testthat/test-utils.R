test_that("user agent", {

  expect_match(mr_user_agent, "R")
  expect_match(mr_user_agent, "mregions2")

  opt_user_agent <- getOption("HTTPUserAgent", default = "")
  expect_match(mr_user_agent, opt_user_agent, fixed = TRUE)
})

test_that("new_mr_df works", {
  expect_error(new_mr_df(1))
  expect_error(new_mr_df(list()))
  expect_error(new_mr_df("not a data frame"))
  expect_error(new_mr_df(data.frame()))

  df <- data.frame(MRGID = 3293)
  df_mr <- new_mr_df(df)
  expect_s3_class(df_mr, "mr_df")

})

test_that("mr_memoise works", {

  .f <- function() invisible(NULL)
  expect_false(memoise::is.memoised(.f))

  f <- mr_memoise(.f)
  expect_true(memoise::is.memoised(f))
})

test_that("not internet msg works", {
  skip("test interactively by disconnecting from network")
  # by using httptest2, we cannot mock the response of things that come from curl::
  # unfortunately there is not an alternative from curl::has_internet at the moment
  #
  # workarrounds are:
  #   -making a function that checks the HEAD of a known service, same that curl::has_internet
  #     But already did this and it gave issues in the CI/CD
  #   -Or change to webmocksr or other http test service, but too much work for only one function
  #
  # For the time being, better to leave it like this.
  expect_error(assert_internet(), "No internet connection")

})

test_that("Server status 500 handled", {

  mock_500 <- function(req) {
    httr2::response(status_code = 500)
  }

  .f <- function(s){
    httr2::with_mock(
      mock_500,
      assert_service(s)
    )}

  s <- "https://geo.vliz.be/geoserver/wfs?request=GetCapabilities"
  expect_error(.f(s), regexp = "500")


  s <- "https://geo.vliz.be/geoserver/MarineRegions/wms?"
  expect_error(.f(s), regexp = "500")


  s <- "https://tiles.emodnet-bathymetry.eu/osm/labels/inspire_quad/1/1/1.png"
  expect_error(.f(s), regexp = "500")


  s <- "https://tiles.emodnet-bathymetry.eu/2020/baselayer/inspire_quad/1/1/1.png"
  expect_error(.f(s), regexp = "500")

})

test_that("Only one filter assertion works",{

  .f <- function() assert_only_one_filter(NULL, NULL)
  expect_null(.f())

  .f <- function() assert_only_one_filter("", NULL)
  expect_null(.f())

  .f <- function() assert_only_one_filter(NULL, "")
  expect_null(.f())

  .f <- function() assert_only_one_filter("", "")
  expect_error(.f())
})
