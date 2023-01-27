use_httptest()

httptest::with_mock_dir("mrp_view", {
  test_that("mrp_view() work", {
    # Note this is hard to check as the WMS service may not be working and we still get an output

    # Test assertions
    .test <- function(layer){
      x <- mrp_view(layer)
      expect_type(x, "list")
      expect_s3_class(x, c("leaflet", "htmlwidget"))
    }

    invisible(lapply(mrp_list()$data_product, .test))

    .f <- function() mrp_view("foo")
    expect_error(.f())

    .f <- function() mrp_view(c("eez", "eez_boundaries"))
    expect_error(.f())

  })
})
