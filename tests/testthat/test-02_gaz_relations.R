test_that("relations work", {

  # config
  bouvet = 8634

  # df
  x <- gaz_search("Bouvet", like = FALSE) %>%
    gaz_relations(type = "administrativepartof", direction = "upper")
  expect_type(x, "list")
  expect_s3_class(x, c("tbl_df", "data.frame"))

  # mrgid
  x <- gaz_relations(
    bouvet,
    with_geometry = TRUE,
    type = "administrativepartof",
    direction = "upper"
  )
  expect_type(x, "list")
  expect_s3_class(x, "sf")
  expect_s3_class(x, c("tbl_df", "data.frame"))

})
