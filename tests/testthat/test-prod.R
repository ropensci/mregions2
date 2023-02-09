use_httptest()

skip_everywhere <- function(){
  skip_if_offline()
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()
}

test_that("Client can be created", {
  skip_everywhere()

  wfs <- .mrp_init_wfs_client()
    expect_type(wfs, "environment")
    expect_s3_class(wfs, "R6")
    expect_s3_class(wfs, "WFSClient")
})

test_that("Client fails with no internet", {
  # we use curl here, httptest cannot record
  expect_error(mrp_init_wfs_client_check_internet(test = TRUE),
               regexp = 'Did you check your internet connection?')
})


test_that("File with info exists", {
  system.file("mrp_list.csv", package = "mregions2", mustWork = TRUE) %>%
    file.exists() %>%
    expect_true()
})

test_that("list funtion works", {
  skip_everywhere()

  x <- mrp_list()
  expect_type(x, "list")
  expect_s3_class(x, c("tbl_df", "data.frame"))
  expect_gt(nrow(x), 1)
})

test_that("mrp_view() works", {
  # Note this is hard to check as the WMS service may not be working and we still get an output
  skip("Check mrp_view interactively")

  x <- mrp_view("eez")
    expect_type(x, "list")
    expect_s3_class(x, c("leaflet", "htmlwidget"))

  # Test error
  .f <- function() assert_emodnet_bathy("https://httpbin.org/status/400")
  expect_error(.f(), "Client error")

  .f <- function() assert_emodnet_bathy("https://httpbin.org/status/500")
  expect_error(.f(), "Server error")

  .f <- function() mrp_view("foo")
  expect_error(.f())

  .f <- function() mrp_view(c("eez", "eez_boundaries"))
  expect_error(.f())
})


# Mocked requests
httptest::with_mock_dir("prod/fail/", {
  test_that("Server status 500 handled", {
    .f <- function() mrp_init_wfs_client_exceptions_handler("https://geo.vliz.be/geoserver/wfs")
    expect_error(.f(), regexp = "500")
  })
})

httptest::with_mock_dir("prod/ok/", {
  test_that("mrp_colnames() works", {

    # Returns a data frame
    x <- .mrp_colnames("ecs_boundaries")
    expect_s3_class(x, c("data.frame"))
    expect_gte(nrow(x), 1)
    expect_gt(ncol(x), 1)

    # Check all columns are of type character
    invisible(apply(x, 2, expect_vector, ptype = character()))

    # Expect errors
    .f <- function() .mrp_colnames("this is not a data product")
    expect_error(.f())

    .f <- function() .mrp_colnames(c("ecs", "eez"))
    expect_error(.f())

    .f <- function() .mrp_colnames(1)
    expect_error(.f())
  })

  test_that("mrp_col_unique() works", {

    # two names for the same function
    expect_true(all.equal(mrp_col_unique, mrp_col_distinct))

    # Expect memoization
    expect_true(memoise::is.memoised(mrp_col_unique))
    expect_false(memoise::is.memoised(.mrp_col_unique))

    # Returns a vector of type character
    x <- .mrp_col_unique("ecs_boundaries", "line_type")
    expect_vector(x, ptype = character())
    expect_gte(length(x), 1)

    # Returns a vector of type numeric
    x <- .mrp_col_unique("ecs_boundaries", "line_id")
    expect_vector(x, ptype = numeric())
    expect_gte(length(x), 1)

    # Returns a vector of type date
    x <- .mrp_col_unique("ecs_boundaries", "doc_date")
    expect_vector(x)
    expect_s3_class(x, "Date")
    expect_gte(length(x), 1)

    # Expect errors
    .f <- function() .mrp_col_unique("this is not a data product", "mrgid")
    expect_error(.f())

    .f <- function() .mrp_col_unique(1, "mrgid")
    expect_error(.f())

    .f <- function() .mrp_col_unique("ecs_boundaries", 1)
    expect_error(.f())

    .f <- function() .mrp_col_unique("ecs_boundaries", "this is not a column")
    expect_error(.f())

    .f <- function() .mrp_col_unique("ecs_boundaries", "the_geom")
    expect_error(.f())
  })


  test_that("mrp_get() works", {
    expect_sf <- function(x){
      expect_type(x, "list")
      expect_s3_class(x, c("sf"))
      expect_s3_class(x, c("data.frame", "tbl_df"))
      expect_gte(nrow(x), 1)
    }

    # Transformation in multipolygon or multilinestring works
    # Returns a sf object
    x <- mrp_get("ecs", cql_filter = "mrgid = 64123") # Mexican ECS Deposit
    expect_sf(x)
    expect_s3_class(sf::st_geometry(x), "sfc_MULTIPOLYGON")

    x <- mrp_get("ecs_boundaries", cql_filter = "line_id = 4232") # Mexican ECS Deposit line
    expect_sf(x)
    expect_s3_class(sf::st_geometry(x), "sfc_MULTILINESTRING")

    # Expect errors
    .f <- function() mrp_get("this product does not exists")
    expect_error(.f())

    .f <- function() mrp_get("ecs_boundaries", cql_filter = "this is not a good filter")
    expect_error(.f())

  })
}, simplify = FALSE)