
testthat::test_that("mr_gaz_records_by_name() gives the output `character`.", {
  name <- "High Seas"
  count <- 5
  tested_result <- mr_gaz_records_by_name(name = name, count = count)
  tested_type <- typeof(test_result)
  tested_class <- class(test_result)

  expected_type <- "character"
  expect_type(tested_type, expected_type)
})

testthat::test_that("mr_gaz_records_by_name() gives an output of the expected dimensions.", {
  name <- "High Seas"
  count <- 5
  tested_result <- mr_gaz_records_by_name(name = name, count = count)

  expected_ncol <- 14
  expect_length(test_result, expected_ncol) # make the 14 more dynamic, if e.g. some columns are left out in the future?

  tested_nrow <- nrow(tested_result)
  expected_nrow <- count
  expect_equal(tested_nrow, count)
})

testthat::test_that("Error message appears when PC is disconnected to the internet.", {
  tested_res <- readLines("https://marineregions.org/gazetteer.php?p=webservices&type=rest", n=1)
  expected_res <- "<!DOCTYPE html>"
  result <- expect_equal(tested_res, expected_res)
  # does not make that much sense just yet.
  # Todo: through comprehensible error message
})

