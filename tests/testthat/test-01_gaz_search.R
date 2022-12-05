# Config
b24nm = 49243
n24nm = 49244

test_that("character method works", {

  # method one name - type ids and fuzzy search
  x <- gaz_search("Belgiun", typeid = c(350, 351), fuzzy = TRUE)
  expect_type(x, "list")
  expect_s3_class(x, c("sf", "tbl_df", "data.frame"))

  # method more than one name - fuzzy search - another language
  x <- gaz_search(c("Belgique", "Framce"), fuzzy = TRUE)
  expect_type(x, "list")
  expect_s3_class(x, c("tbl_df", "data.frame"))

  # with geometries work
  x <- gaz_search("Belgian 24 NM", with_geometry = TRUE)
  expect_type(x, "list")
  expect_s3_class(x, c("sf", "tbl_df", "data.frame"))

  x <- gaz_search(c("Belgian 24 NM", "Dutch 24 NM"), with_geometry = TRUE)
  expect_type(x, "list")
  expect_s3_class(x, c("sf", "tbl_df", "data.frame"))

  # error - not fuzzy
  expect_no_error(gaz_search("Belgie", language = "nl", fuzzy = TRUE))
  expect_error(gaz_search("Belgie", language = "nl", fuzzy = FALSE))

  # error - wrong language

  # error - typeid not in list
  expect_error(gaz_search("Belgium", typeid = 999999999))

})


test_that("mrgid lookup method works", {

  # rdf
  x <- gaz_search(c(b24nm, n24nm), rdf = TRUE)
  expect_type(x, "list")
  expect_s3_class(x, "rdf")

  # df
  x <- gaz_search(c(b24nm, n24nm))
  expect_type(x, "list")
  expect_s3_class(x, c("tbl_df", "data.frame"))

  # with geometry
  x <- gaz_search(b24nm, with_geometry = FALSE)
  expect_type(x, "list")
  expect_s3_class(x, c("sf", "tbl_df", "data.frame"))

  x <- gaz_search(c(b24nm, n24nm), with_geometry = FALSE)
  expect_type(x, "list")
  expect_s3_class(x, c("sf", "tbl_df", "data.frame"))

  # error - fake mrgid
  expect_error(gaz_search(999999999))

})


test_that("intersection method works", {
  # point near belgium
  lon = 2.927
  lat = 51.21551

  # df
  x <- gaz_search(x = lon, y = lat)
  expect_type(x, "list")
  expect_s3_class(x, c("tbl_df", "data.frame"))

  # filter on typeid and add geometries
  x <- gaz_search(x = lon, y = lat, typeid = c(350, 351), with_geometry = TRUE)
  expect_type(x, "list")
  expect_s3_class(x, c("sf"))
  expect_s3_class(x, c("sf", "tbl_df", "data.frame"))

  # Signature sfg
  point <- sf::st_point(c(lon, lat))
  x <- gaz_search(x = point, typeid = c(350, 351))
  expect_type(x, "list")
  expect_s3_class(x, c("tbl_df", "data.frame"))

  # Signature sfc
  point <- sf::st_sfc(point, crs = 4326)
  x <- gaz_search(x = point, typeid = c(350, 351))
  expect_type(x, "list")
  expect_s3_class(x, c("tbl_df", "data.frame"))

  # Signature sf
  point <- data.frame(point) %>% sf::st_as_sf()
  x <- gaz_search(x = point, typeid = c(350, 351))
  expect_type(x, "list")
  expect_s3_class(x, c("tbl_df", "data.frame"))

  # error - out of boundaries
  expect_error(gaz_search(x = 181, y = 90))
  expect_error(gaz_search(x = 180, y = 91))
  expect_error(gaz_search(x = -181, y = -90))
  expect_error(gaz_search(x = -180, y = -91))

  # error - wrong latitude data type
  expect_error(gaz_search(x = 180, y = "90"))

})

