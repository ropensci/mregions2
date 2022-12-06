test_that("mr_products() work", {

  x <- mr_products()
  expect_type(x, "list")
  expect_s3_class(x, c("tbl_df", "data.frame"))
  expect_gte(nrow(x), 1)

})

test_that("mr_view() work", {
  # Note this is hard to check as the WMS service may not be working and we still get an output

  # Test assertions
  .test <- function(layer){
    x <- mr_view(layer)
    expect_type(x, "list")
    expect_s3_class(x, c("leaflet", "htmlwidget"))
  }

  invisible(lapply(mr_products()$layer, .test))

  .f <- function() mr_view("foo")
  expect_error(.f())

  .f <- function() mr_view(c("eez", "eez_boundaries"))
  expect_error(.f())

})
