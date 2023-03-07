test_that("gaz_search.numeric", {
  gaz_search(17) %>% expect_mr()
  gaz_search(14, rdf = TRUE) %>% expect_s3_class("rdf")

  gaz_search(999999999) %>% expect_error()
})

test_that("gaz_search.character", {
  gaz_search("Belgium") %>% expect_mr()
  gaz_search("Belgiun", fuzzy = TRUE) %>% expect_mr()
  gaz_search(c("Belgique", "Framce"), fuzzy = TRUE) %>% expect_mr()

  gaz_search("Belgiun", fuzzy = FALSE) %>% expect_error()
  gaz_search("Belgie", language = "es") %>% expect_error()
  gaz_search("Belgium", typeid = 999999999) %>% expect_error()

})

test_that("gaz_search.sfg", {
  # Config
  lon = 2.927
  lat = 51.21551
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

  # error - wrong latitude data type
  expect_error(gaz_search(x = 180, y = "90"))
})

test_that("gaz_search_by_source",{
  gaz_search_by_source(77) %>% expect_mr()
  gaz_search_by_source("Belgian Sea Fisheries") %>% expect_mr()

  gaz_search_by_source("This is not a source") %>% expect_error()
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

})

# Replace geometries with dummy polygons manually
# Dummy polygon: POLYGON ((1 0, 1 1, 0 1, 0 0, 1 0))
test_that("gaz_geometry.numeric", {

  gaz_geometry(b24nm) %>% expect_s3_class("sfc")
  gaz_geometry(b24nm, format = "rdf") %>% expect_s3_class("rdf")
  gaz_geometry(b24nm, format = "wkt") %>% wk::as_wkt() %>% expect_s3_class("wk_wkt")

  x <- gaz_geometry(b24nm, format = "sf")
  expect_s3_class(x, "sf")
  expect_true("MRGID" %in% names(x))

  x <- gaz_geometry(med_east, multipart = FALSE)
  expect_s3_class(x, "sfc")
  expect_gt(length(x), 1)

  x <- gaz_geometry(med_east, multipart = TRUE)
  expect_s3_class(x, "sfc")
  expect_length(x, 1)

  gaz_geometry(kuurne, multipart = TRUE) %>% expect_warning()
  gaz_geometry(999999999) %>% expect_error()
  gaz_geometry(bouvet_eez) %>% expect_error()

})

test_that("gaz_geometry.mr_df", {
  gaz_search(b24nm) %>% gaz_geometry() %>% expect_mr_sf()
  gaz_search(b24nm, with_geometry = TRUE) %>% expect_mr_sf()

  gaz_search(mvb) %>% # fails normally cause empty request - done in other mrgid and replace manually
    gaz_geometry() %>%
    sf::st_geometry() %>%
    expect_s3_class("sfc_POINT")

  data.frame() %>% gaz_geometry() %>%  expect_error()
  gaz_search(bouvet_eez) %>% gaz_geometry() %>% expect_error()

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

})
