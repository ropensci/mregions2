use_httptest2()

httptest2::with_mock_dir("gaz_rest_wmses", {
  test_that("gaz_rest_wmses works", {

    x <- gaz_rest_wmses(3293)
    expect_type(x, "list")
    expect_s3_class(x, c("tbl_df", "data.frame"))

    .f <- function() gaz_rest_wmses("this is not an mrgid")
    expect_error(.f())

    .f <- function() gaz_rest_wmses(1:2)
    expect_error(.f())
  })
})

httptest2::with_mock_dir("gaz_rest_names_by_mrgid", {
  test_that("gaz_rest_names_by_mrgid works", {

    x <- gaz_rest_names_by_mrgid(3293)
    expect_type(x, "character")
    expect_gte(length(x), 1)

    .f <- function() gaz_rest_names_by_mrgid("give me a name")
    expect_error(.f())

    .f <- function() gaz_rest_names_by_mrgid(1:2)
    expect_error(.f())

  })
})
