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


test_that("mrp_view() leaflet output works", {
  map <- mrp_view("eez")

  expect_s3_class(map, "leaflet")
  expect_s3_class(map, "htmlwidget")

  map <- unclass(map)

  map_crs <- map[["x"]][["options"]][["crs"]][["crsClass"]]
  expect_equal(map_crs, "L.CRS.EPSG4326")

  map_calls <- map[["x"]][["calls"]]

  # Step 1: Test EMODnet Bathymetry Tiles
  map_call_bath <- map_calls[[1]]

  method <- map_call_bath[["method"]]
  expect_equal(method, "addTiles")

  checkmate_url <- "https://tiles.emodnet-bathymetry.eu/2020/baselayer/inspire_quad/{z}/{x}/{y}.png"
  test_url <- map_call_bath[["args"]][[1]]
  expect_equal(test_url, checkmate_url)

  tms <- map_call_bath[["args"]][[4]]$tms
  expect_false(tms)

  attribution <- map_call_bath[["args"]][[4]]$attribution
  expect_match(attribution, "EMODnet", fixed = TRUE)

  # Step 2: Test WMS
  map_call_wms <- map_calls[[2]]

  method <- map_call_wms[["method"]]
  expect_equal(method, "addWMS")

  checkmate_url <- "https://geo.vliz.be/geoserver/MarineRegions/wms?"
  test_url <- map_call_wms[["args"]][[1]]
  expect_equal(test_url, checkmate_url)

  format <- map_call_wms[["args"]][[4]]$format
  expect_equal(format, "image/png")

  transparent <- map_call_wms[["args"]][[4]]$transparent
  expect_true(transparent)

  info_format <- map_call_wms[["args"]][[4]]$info_format
  expect_equal(info_format, "text/html")

  attribution <- map_call_wms[["args"]][[4]]$attribution
  expect_match(attribution, "Marine Regions", fixed = TRUE)

  # Step 3: Test EMODnet Bathymetry labels
  map_call_labs <- map_calls[[3]]

  method <- map_call_labs[["method"]]
  expect_equal(method, "addTiles")

  checkmate_url <- "https://tiles.emodnet-bathymetry.eu/osm/labels/inspire_quad/{z}/{x}/{y}.png"
  test_url <- map_call_labs[["args"]][[1]]
  expect_equal(test_url, checkmate_url)

  tms <- map_call_labs[["args"]][[4]]$tms
  expect_false(tms)

})

test_that("mrp_view() filters can be passed", {
  skip_on_cran()

  # OGC filter
  filter <- "<Filter>
              <PropertyIsEqualTo>
                <PropertyName>mrgid</PropertyName>
                <Literal>3293</Literal>
              </PropertyIsEqualTo>
             </Filter>"

  map <- mrp_view("eez", filter = filter)
  expect_s3_class(map, "leaflet")
  expect_s3_class(map, "htmlwidget")

  map_url <- map[["x"]][["calls"]][[2]][["args"]][[1]]
  parsed_url <- httr2::url_parse(map_url)
  parsed_filter <- parsed_url$query$filter
  expect_equal(parsed_filter, filter)

  # CQL filter
  cql_filter <- "mrgid=3293"

  map <- mrp_view("eez", cql_filter = cql_filter)
  expect_s3_class(map, "leaflet")
  expect_s3_class(map, "htmlwidget")

  map_url <- map[["x"]][["calls"]][[2]][["args"]][[1]]
  parsed_url <- httr2::url_parse(map_url)
  parsed_cql_filter <- parsed_url$query$cql_filter
  expect_equal(parsed_cql_filter, cql_filter)
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


  mock_500 <- function(req) {
    httr2::response(status_code = 500)
  }
  .f <- function(){
    httr2::with_mocked_responses(
      mock_500,
      mrp_view("eez")
    )}
  expect_error(.f(), regexp = "500")

  withr::local_envvar("TESTPKG.NOINTERNET" = "blop")
  expect_error(mrp_view("eez"), "No internet connection")
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

  .f <- function() mrp_get("eez", path = "this path does not exist")
  expect_error(.f())

  withr::local_envvar("TESTPKG.NOINTERNET" = "blop")
  expect_error(mrp_get("eez"), "No internet connection")
})


httptest2::with_mock_dir("prod/fail/", {
  test_that("mrp_get: Bad filters errors surfaced", {
    withr::local_options("mregions2.download_path" = "./prod/fail/geo")
    withr::local_envvar("TESTPKG.ISTEST" = "true")
    withr::local_envvar("TESTPKG.CACHETIME" = 0)

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

  test_that("mrp_get: Warnings coming from the server are surfaced", {
    withr::local_options("mregions2.download_path" = "./prod/fail/geo")
    withr::local_envvar("TESTPKG.ISTEST" = "true")
    withr::local_envvar("TESTPKG.CACHETIME" = 0)

    .f <- function() mrp_get("eez", cql_filter = "mrgid = -1")
    expect_warning(.f(), regexp = "empty", fixed = TRUE)
  })


})

httptest2::with_mock_dir("prod/ok/", {
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
    withr::local_options("mregions2.download_path" = "./prod/ok/geo")
    withr::local_envvar("TESTPKG.ISTEST" = "true")

    expect_sf <- function(x){
      expect_type(x, "list")
      expect_s3_class(x, c("sf"))
      expect_s3_class(x, c("data.frame", "tbl_df"))
      expect_gte(nrow(x), 1)
    }


    # Mock HTTP request like if there was no cache
    withr::local_envvar("TESTPKG.CACHETIME" = 0)

    # Mexican ECS Deposit
    .f1 <- function() mrp_get("ecs", cql_filter = "mrgid = 64123")
    expect_sf(.f1())
    expect_s3_class(sf::st_geometry(.f1()), "sfc_POLYGON")

    # Mexican ECS Deposit line
    .f2 <- function() mrp_get("ecs_boundaries", cql_filter = "line_id = 4232")
    expect_sf(.f2())
    expect_s3_class(sf::st_geometry(.f2()), "sfc_LINESTRING")

    # Actually reading from cache without HTTP request
    withr::local_envvar("TESTPKG.CACHETIME" = Inf)

    expect_message(.f1(), "Cache", fixed = TRUE)
    expect_sf(.f1())
    expect_s3_class(sf::st_geometry(.f1()), "sfc_POLYGON")

    expect_message(.f2(), "Cache", fixed = TRUE)
    expect_sf(.f2())
    expect_s3_class(sf::st_geometry(.f2()), "sfc_LINESTRING")
  })

}, simplify = TRUE)


