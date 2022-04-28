usethis::use_package("testthat", type = "Suggests")


testthat::test_that("req_URL() provides a base URL for a http request", {
  expect_equal(req_URL(file_type = "json", method = "getGazetteerRecordsByName"), "https://marineregions.org//rest/getGazetteerRecordsByName.json")
})

# Question: outsource assertions from re-URL.R to test-req_URL.R?

