test_that("gaz_rest_wmses works", {

  x <- gaz_rest_wmses(3293)
  expect_type(x, "list")
  expect_s3_class(x, c("tbl_df", "data.frame"))

  .f <- function() gaz_rest_wmses("this is not an mrgid")
  expect_error(.f())

  .f <- function() gaz_rest_wmses(1:2)
  expect_error(.f())




})

test_that("gaz_rest_names_by_mrgid works", {

  x <- gaz_rest_names_by_mrgid(14)
  expect_type(x, "character")
  expect_gte(length(x), 1)

  .f <- function() gaz_rest_names_by_mrgid("give me a name")
  expect_error(.f())

  .f <- function() gaz_rest_names_by_mrgid(1:2)
  expect_error(.f())

})
