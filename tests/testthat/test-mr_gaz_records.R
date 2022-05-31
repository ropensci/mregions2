
testthat::test_that("mr_gaz_records_by_name() gives the output `character`.", {
  name <- "High Seas"
  count <- 5
  tested_result <- mr_gaz_records_by_name(name = name, count = count)
  tested_type <- typeof(tested_result)
  tested_class <- class(tested_result)

  expected_type <- "character"
  expect_type(tested_type, expected_type)
})

testthat::test_that("mr_gaz_records_by_name() gives an output of the expected dimensions.", {
  name <- "High Seas"
  count <- 5
  tested_result <- mr_gaz_records_by_name(name = name, count = count)

  expected_ncol <- 14
  expect_length(tested_result, expected_ncol) # make the 14 more dynamic, if e.g. some columns are left out in the future?

  tested_nrow <- nrow(tested_result)
  expected_nrow <- count
  expect_equal(tested_nrow, count)
})
