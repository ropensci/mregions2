# Config
skip_everywhere <- function(){
  skip_if_offline()
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()
}

# Perform

test_that("Product list exists", {
  system.file("mrp_list.csv", package = "mregions2", mustWork = TRUE) %>%
    file.exists() %>%
    expect_true()

  x <- mrp_list
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
})

test_that("mrp_view() assertions work", {
  .f <- function() assert_deps(c("thisisa", "fakepackagename"))
  expect_error(.f(), "not installed")

  .f <- function() mrp_view(1)
  expect_error(.f())

  .f <- function() mrp_view("foo")
  expect_error(.f())

  .f <- function() mrp_view(c("eez", "eez_boundaries"))
  expect_error(.f())

  .f <- function() mrp_view("eez",
                            filter = "<Filter>
                                        <PropertyIsEqualTo>
                                          <PropertyName>mrgid</PropertyName>
                                          <Literal>3293</Literal>
                                        </PropertyIsEqualTo>
                                      </Filter>",
                            cql_filter = "mrgid=3293")
  expect_error(.f())

  .f <- function() mrp_view("eez", filter = 1)
  expect_error(.f())

  .f <- function() mrp_view("eez", cql_filter = 1)
  expect_error(.f())
})

test_that("mrp_get() assertions work", {
  .f <- function() mrp_get(1)
  expect_error(.f())

  .f <- function() mrp_get("foo")
  expect_error(.f())

  .f <- function() mrp_get(c("eez", "eez_boundaries"))
  expect_error(.f())

  .f <- function() mrp_get("eez",
                            filter = "<Filter>
                                        <PropertyIsEqualTo>
                                          <PropertyName>mrgid</PropertyName>
                                          <Literal>3293</Literal>
                                        </PropertyIsEqualTo>
                                      </Filter>",
                            cql_filter = "mrgid=3293")
  expect_error(.f())

  .f <- function() mrp_get("eez", filter = 1)
  expect_error(.f())

  .f <- function() mrp_get("eez", cql_filter = 1)
  expect_error(.f())

  .f <- function() mrp_get("eez", count = "1")
  expect_error(.f())
})

test_that("Server status 500 handled", {

  mock_500 <- function(req) {
    httr2::response(status_code = 500)
  }

  .f <- function(s){
    httr2::with_mock(
      mock_500,
      assert_service(s)
    )}

  s <- "https://geo.vliz.be/geoserver/wfs?request=GetCapabilities"
  expect_error(.f(s), regexp = "500")


  s <- "https://geo.vliz.be/geoserver/MarineRegions/wms?"
  expect_error(.f(s), regexp = "500")


  s <- "https://tiles.emodnet-bathymetry.eu/osm/labels/inspire_quad/1/1/1.png"
  expect_error(.f(s), regexp = "500")


  s <- "https://tiles.emodnet-bathymetry.eu/2020/baselayer/inspire_quad/1/1/1.png"
  expect_error(.f(s), regexp = "500")

})

httptest2::with_mock_dir("prod/fail/", {
  test_that("Bad filters errors surfaced", {

    .f <- function() mrp_get("eez", filter="<Filter>")
    expect_error(.f(), "XML getFeature request SAX parsing error")

    .f <- function() mrp_get("eez", filter="<Filter>")
    expect_error(.f(), "XML getFeature request SAX parsing error")

    .f <- function(){
      mrp_get("eez", filter="
        <Filter>
          <PropertyIsEqualTo>
            <PropertyName>notvalidparameter</PropertyName>
            <Literal>3293</Literal>
          </PropertyIsEqualTo>
        </Filter>")
    }
    expect_error(.f(), "InvalidParameterValue")

    .f <- function() mrp_get("eez", cql_filter ="notvalidparameter=3293")
    expect_error(.f(), "InvalidParameterValue")

    .f <- function() mrp_get("eez", cql_filter="mrgid='cannotcast'")
    expect_error(.f(), "NoApplicableCode")

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
  })

  test_that("mrp_colnames() fails nicely",{
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
  })

  test_that("mrp_col_unique() fails nicely",{
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
