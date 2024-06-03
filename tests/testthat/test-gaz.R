# Config
expect_mr <- function(x){
  expect_type(x, "list")
  expect_s3_class(x, "mr_df")
  expect_s3_class(x, c("tbl_df", "data.frame"))
  expect_true("MRGID" %in% names(x))
}

expect_mr_sf <- function(x){
  expect_mr(x)
  expect_s3_class(x, "sf")
}

mock_404 <- function(req) httr2::response(404)
mock_500 <- function(req) httr2::response(500)


belgium <- 14
france <- 17
b24nm <- 49243
med_east <- 4280 # several polygons, same source PROBLEM
kuurne <- 58 # several polygons, two sources
bouvet_eez <- 8399 # deleted
mvb <- 30666 # only centroid
rocks_placetype <- 193

# Perform
httptest2::with_mock_dir("gaz", {
  test_that("gaz_search.numeric", {
    gaz_search(17) %>% expect_mr()
    gaz_search(c(14, 17), rdf = TRUE) %>% expect_s3_class("rdf")

    gaz_search(999999999) %>% expect_error()

    .f <- function() gaz_search(17, rdf = TRUE, with_geometry = TRUE)
    expect_warning(.f(), "ignored", fixed = TRUE)
  })

  test_that("gaz_search.character", {
    gaz_search("Belgium") %>% expect_mr()
    gaz_search("Belgiun", fuzzy = TRUE) %>% expect_mr()
    gaz_search(c("Belgique", "Framce"), fuzzy = TRUE) %>% expect_mr()

    gaz_search("Belgiun", fuzzy = FALSE) %>% expect_error()
    gaz_search("test_like_is_false", like = FALSE) %>% expect_error()
    gaz_search("Belgie", language = "es") %>% expect_error()
    gaz_search("Belgie", language = "xx") %>% expect_error()
    gaz_search("Belgium", typeid = 999999999) %>% expect_error()
    gaz_search("test_not_found", fuzzy = TRUE, like = TRUE) %>% expect_error()

    gaz_search("Belgium", typeid = rocks_placetype) %>%
      expect_error("type", fixed = TRUE)

    .f <- function() httr2::with_mocked_responses(mock_500, gaz_search("Belgium"))
    expect_error(.f(), regexp = "500", fixed = TRUE)


  })

  test_that("gaz_search.sfg", {
    # Config
    lon <- 2.927
    lat <- 51.21551
    sfg <- sf::st_point(c(lon, lat))
    sfc <- sf::st_sfc(sfg, crs = 4326)
    sf <-  sf::st_as_sf(data.frame(sfc))

    # Test
    gaz_search(x = lon, y = lat) %>% expect_mr()
    gaz_search(x = lon, y = lat, typeid = c(350, 351)) %>% expect_mr()
    gaz_search(sfg) %>% expect_mr()
    gaz_search(sfc) %>% expect_mr()
    gaz_search(sf) %>% expect_mr()

    expect_error(gaz_search(x = 181, y = 90))
    expect_error(gaz_search(x = 180, y = 91))
    expect_error(gaz_search(x = -181, y = -90))
    expect_error(gaz_search(x = -180, y = -91))
    expect_error(gaz_search(x = 180, y = "90"))

    gaz_search(x = lon, y = lat, typeid = 999999999) %>%
      expect_error("must be element of set", fixed = TRUE)

    .f <- function() httr2::with_mocked_responses(mock_404, gaz_search(x = lon, y = lat))
    expect_error(.f(), regexp = "no matches", fixed = TRUE)

    .f <- function() httr2::with_mocked_responses(mock_500, gaz_search(x = lon, y = lat))
    expect_error(.f(), regexp = "500", fixed = TRUE)

    gaz_search(x = lon, y = lat, typeid = rocks_placetype) %>%
      expect_error("type", fixed = TRUE)

  })

  test_that("gaz_search_by_source",{
    gaz_search_by_source(77) %>% expect_mr()
    gaz_search_by_source("Belgian Sea Fisheries") %>% expect_mr()

    gaz_search_by_source("This is not a source") %>% expect_error()

    gaz_search_by_source(53) %>%
      expect_error(regexp = "No records", fixed = TRUE)

    .f <- function(){
      httr2::with_mocked_responses(mock_500,
                       gaz_rest_records_by_source("Belgian Sea Fisheries"))
    }
    expect_error(.f(), regexp = "500", fixed = TRUE)
  })

  test_that("gaz_sources",{
    x <- gaz_sources()
    expect_s3_class(x, c("tbl_df", "data.frame"))
    expect_gt(nrow(x), 1)
    expect_false(any(is.na(x$source)))
    expect_false(any(is.na(x$sourceID)))
  })

  test_that("gaz_search_by_type", {
    gaz_search_by_type(c("Wreck", "Diving spot")) %>% expect_mr()
    gaz_search_by_type(c(220, 195)) %>% expect_mr()

    gaz_search_by_type("this is not a type") %>% expect_error()
  })

  test_that("gaz_types",{
    x <- gaz_types()
    expect_s3_class(x, c("tbl_df", "data.frame"))
    expect_gt(nrow(x), 1)
    expect_false(any(is.na(x$type)))
    expect_false(any(is.na(x$typeID)))
  })

  test_that("gaz_relations", {
    gaz_relations(b24nm, type = "partof", direction = "upper") %>%
      expect_mr()

    gaz_search(b24nm) %>%
      gaz_relations(type = "partof", direction = "upper") %>%
      expect_mr()

    .f <- function() gaz_relations(b24nm, type = "fake type")
    expect_error(.f(), regexp = "Assertion", fixed = TRUE)

    .f <- function() gaz_relations(b24nm, type = c("partof", "administrativepartof"))
    expect_error(.f(), regexp = "Assertion", fixed = TRUE)

    .f <- function() gaz_relations(b24nm, type = 1)
    expect_error(.f(), regexp = "Assertion", fixed = TRUE)

    .f <- function() gaz_relations(b24nm, direction = "fake direction")
    expect_error(.f(), regexp = "Assertion", fixed = TRUE)

    .f <- function() gaz_relations(b24nm, direction = c("lower", "upper"))
    expect_error(.f(), regexp = "Assertion", fixed = TRUE)

    .f <- function() gaz_relations(b24nm, direction = 1)
    expect_error(.f(), regexp = "Assertion", fixed = TRUE)

    .f <- function() gaz_relations(b24nm, type = "influencedby", direction = "lower")
    expect_error(.f(), regexp = "No relations found", fixed = TRUE)

    .f <- function() httr2::with_mocked_responses(mock_404, gaz_relations(b24nm))
    expect_error(.f(), regexp = "does not exist", fixed = TRUE)

  })

  # Replace geometries with dummy polygons manually
  # Dummy polygon: POLYGON ((1 0, 1 1, 0 1, 0 0, 1 0))
  test_that("gaz_geometry.numeric", {

    gaz_geometry(b24nm) %>% expect_s3_class("sfc")
    gaz_geometry(b24nm, format = "wkt") %>% wk::as_wkt() %>% expect_s3_class("wk_wkt")
    gaz_geometry(c(b24nm, kuurne), format = "rdf") %>% expect_s3_class("rdf")

    x <- gaz_geometry(b24nm, format = "sf")
      expect_s3_class(x, "sf")
      expect_true("MRGID" %in% names(x))

    x <- gaz_geometry(med_east, multipart = FALSE)
      expect_s3_class(x, "sfc")
      expect_gt(length(x), 1)

    x <- gaz_geometry(med_east, multipart = TRUE)
      expect_s3_class(x, "sfc")
      expect_length(x, 1)

    gaz_geometry(1.5) %>% expect_error("Assertion")
    gaz_geometry(-1) %>% expect_error("Assertion")


    gaz_geometry(kuurne, multipart = TRUE) %>% expect_warning()
    gaz_geometry(999999999) %>% expect_error()

  })

  test_that("gaz_rest_geometry (not exported) is tested", {
    gaz_rest_geometry(b24nm, sourceid = 1.5) %>% expect_error("Assertion")
    gaz_rest_geometry(b24nm, sourceid = -1) %>% expect_error("Assertion")
    gaz_rest_geometry(kuurne, sourceid = 182,
                      attribute_value = 34023, format = "sfc") %>%
      expect_s3_class("sfc")
  })

  test_that("gaz_geometry.mr_df", {
    gaz_search(b24nm) %>% gaz_geometry() %>% expect_mr_sf()
    gaz_search(b24nm, with_geometry = TRUE) %>% expect_mr_sf()

    gaz_search(mvb) %>% # fails normally cause empty request - done in other mrgid and replace manually
      gaz_geometry() %>%
      sf::st_geometry() %>%
      expect_s3_class("sfc_POINT")

    # gaz_add_geometry is the underlying service behind the mr_df method
    data.frame() %>% gaz_add_geometry() %>%
      expect_error("Assertion", fixed = TRUE)

    # test on dummy df for difficult cases
    dummy_df <- data.frame(MRGID = mvb, preferredGazetteerName = "dummy",
                           status = "standard", accepted = 1,
                           stringsAsFactors = FALSE) %>% new_mr_df()

    dummy_point <- dummy_df %>% dplyr::mutate(latitude = 0, longitude = 0) %>%
      gaz_geometry()
    dummy_point %>% expect_mr_sf()
    dummy_point %>% sf::st_geometry() %>% expect_s3_class("sfc_POINT")

    dummy_bbox <- dummy_df %>% dplyr::mutate(minLatitude = 0, minLongitude = 0,
                                             maxLatitude = 1, maxLongitude = 1,
                                             ) %>% gaz_geometry()
    dummy_bbox %>% expect_mr_sf()
    dummy_bbox %>% sf::st_geometry() %>% expect_s3_class("sfc_POLYGON")

  })

  test_that("gaz_rest_wmses", {
    x <- gaz_rest_wmses(3293)
    expect_type(x, "list")
    expect_s3_class(x, c("tbl_df", "data.frame"))

    .f <- function() gaz_rest_wmses("this is not an mrgid")
    expect_error(.f())

    .f <- function() gaz_rest_wmses(1:2)
    expect_error(.f())
  })

  test_that("gaz_rest_names_by_mrgid", {
    x <- gaz_rest_names_by_mrgid(3293)
    expect_type(x, "character")
    expect_gte(length(x), 1)

    .f <- function() gaz_rest_names_by_mrgid("fake name")
    expect_error(.f())

    .f <- function() gaz_rest_names_by_mrgid(1:2)
    expect_error(.f())

    .f <- function() gaz_rest_names_by_mrgid(999999999)
    expect_error(.f(), regexp = "does not exist", fixed = TRUE)

    .f <- function() assert_mrgid_exists(3293)
    expect_invisible(.f())

    .f <- function() assert_mrgid_exists(999999999)
    expect_error(.f(), regexp = "does not exist", fixed = TRUE)
  })

}, simplify = TRUE)


