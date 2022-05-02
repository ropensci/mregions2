
testthat::test_that("req_URL() has valid input arguments", {
  test_url <- req_URL(api_type = "rest", file_format = "json", method = "getGazetteerRecordsByName")
  expect_url <- "https://marineregions.org//rest/getGazetteerRecordsByName.json/"
  expect_equal(test_url, expect_url)


# testthat "req_URL() is a valid URL"
  test_name <- "High Seas"

  test_req <- httr2::request(test_url) %>%
    # httr2::req_headers(accept = "application/json") %>%
    httr2::req_url_path_append(URLencode(test_name)) %>%
    httr2::req_url_path_append("/")
  test_resp <- test_req %>% httr2::req_perform()

  status_ok <- 200
  testthat::expect_equal(test_resp$status_code, status_ok)
})
