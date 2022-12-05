# Config
b24nm = 49243
n24nm = 49244
med_east = 4280 # several polygons, same source
kuurne = 58 # several polygons, two sources
bouvet_eez = 8399 # deleted
la_rinconada = 22550 # only centroid

test_that("numeric method works", {
  skip_if_offline()

  # numeric
  x <- gaz_geometry(c(b24nm, n24nm))
  expect_type(x, "list")
  expect_s3_class(x, "sfc")

  x <- gaz_geometry(c(b24nm, n24nm), format = "sf")
  expect_type(x, "list")
  expect_s3_class(x, "sf")
  expect_true("MRGID" %in% names(x))

  x <- gaz_geometry(c(b24nm, n24nm), format = "rdf")
  expect_type(x, "list")
  expect_s3_class(x, "rdf")

  x <- gaz_geometry(c(b24nm, n24nm), format = "wkt")
  expect_type(x, "character")
  expect_s3_class(wk::as_wkt(x), "wk_wkt")

  x <- gaz_geometry(med_east, multipart = FALSE)
  expect_type(x, "list")
  expect_s3_class(x, "sfc")
  expect_gt(length(x), 1)

  x <- gaz_geometry(med_east, multipart = TRUE)
  expect_type(x, "list")
  expect_s3_class(x, "sfc")
  expect_length(x, 1)

  # warning if different sources
  expect_warning(gaz_geometry(kuurne))

  # fail if mrgid doesn't exists or is deleted - no geometry
  expect_error(gaz_geometry(999999999))
  expect_error(gaz_geometry(bouvet_eez))

})

test_that("data.frame method works", {
  skip_if_offline()

  # data.frame
  x <- gaz_search(c(b24nm, la_rinconada)) %>% gaz_geometry()
  expect_type(x, "list")
  expect_s3_class(x, "sf")
  expect_s3_class(x, "data.frame")
  expect_s3_class(x, "tbl_df")

  # if only point available, add
  x <- gaz_search(la_rinconada) %>% gaz_geometry()
  expect_s3_class(sf::st_geometry(x), "sfc_POINT")

  # error if passing random dataframe
  expect_error(data.frame() %>% gaz_geometry())
  expect_error(data.frame(foo = 1) %>% gaz_geometry())

  # error when passing a deleted mrgid
  expect_error(gaz_search(bouvet_eez) %>% gaz_geometry())

})
