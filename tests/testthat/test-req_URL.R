
testthat::test_that("req_URL() has valid input arguments", {
  tested_url <- req_URL(api_type = "rest", file_format = "json", method = "getGazetteerRecordsByName")
  expected_url <- "https://marineregions.org//rest/getGazetteerRecordsByName.json/"
  expect_equal(tested_url, expected_url)
})


testthat::test_that("req_URL() gives a valid URL", {
  tested_url <- req_URL(api_type = "rest", file_format = "json", method = "getGazetteerRecordsByName")
  tested_name <- "High Seas"
  tested_req <- httr2::request(tested_url) %>%
    # httr2::req_headers(accept = "application/json") %>%
    httr2::req_url_path_append(utils::URLencode(tested_name)) %>%
    httr2::req_url_path_append("/")
  tested_resp <- tested_req %>% httr2::req_perform()
  status_ok <- 200
  testthat::expect_equal(tested_resp$status_code, status_ok)
})
