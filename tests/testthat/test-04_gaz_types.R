httptest2::with_mock_dir("gaz_type", {
  test_that("search by placetype works", {

    # Test types
    x <- gaz_types()
    expect_s3_class(x, c("tbl_df", "data.frame"))
    expect_gt(nrow(x), 1)
    expect_false(any(is.na(x$type)))
    expect_false(any(is.na(x$typeID)))

    # Test search by type
    x <- gaz_search_by_type(c("Wreck", "Diving spot"))
    expect_type(x, "list")
    expect_s3_class(x, "mr_df")
    expect_s3_class(x, c("tbl_df", "data.frame"))

    # Errors - placetype not available
    .f <- function() gaz_search_by_type("this is not a type")
    expect_error(.f())

  })
})
