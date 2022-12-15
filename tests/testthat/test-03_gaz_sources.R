test_that("source search works", {

  difficult_text_src <- c(
    "Van Eck, B.T.M. (Ed.) (1999). De Scheldeatlas: een beeld van een estuarium. Rijksinstituut voor Kust en Zee/Schelde InformatieCentrum: Middelburg. ISBN 90-369-3434-6. 120 pp.",
    "Instituto Geográfico Nacional de la República Argentina"
  )

  # Test sources
  x <- gaz_sources()
  expect_s3_class(x, c("tbl_df", "data.frame"))
  expect_gt(nrow(x), 1)
  expect_false(any(is.na(x$source)))
  expect_false(any(is.na(x$sourceID)))

  # Test source search
  x <- gaz_search_by_source(101:102, with_geometry = TRUE)
  expect_type(x, "list")
  expect_s3_class(x, "mr_df")
  expect_s3_class(x, "sf")
  expect_s3_class(x, c("tbl_df", "data.frame"))

  x <- gaz_search_by_source(difficult_text_src)
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
