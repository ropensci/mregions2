httptest2::with_mock_dir("gaz_sources", {
  test_that("source search works", {

    # Test sources
    x <- gaz_sources()
    expect_s3_class(x, c("tbl_df", "data.frame"))
    expect_gt(nrow(x), 1)
    expect_false(any(is.na(x$source)))
    expect_false(any(is.na(x$sourceID)))

    # Test source search
    x <- gaz_search_by_source(77)
    expect_type(x, "list")
    expect_s3_class(x, "mr_df")
    expect_s3_class(x, c("tbl_df", "data.frame"))

    x <- gaz_search_by_source("Belgian Sea Fisheries")
    expect_type(x, "list")
    expect_s3_class(x, "mr_df")
    expect_s3_class(x, c("tbl_df", "data.frame"))

    # Errors - geometry not available
    .f <- function() gaz_search_by_source(100, with_geometry = TRUE)
    expect_error(.f())

    # Errors - fake source
    .f <- function() gaz_search_by_source("This is not a source")
    expect_error(.f())

  })
})
