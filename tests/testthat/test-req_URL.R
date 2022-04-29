
testthat::test_that("req_URL() provides a base URL for a http request", {
  test_url <- req_URL(api_type = "rest", file_format = "json", method = "getGazetteerRecordsByName")
  expect_url <- "https://marineregions.org//rest/getGazetteerRecordsByName.json/"
  expect_equal(test_url, expect_url)
  # test if URL is a string or a URL
})
