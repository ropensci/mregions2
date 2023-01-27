use_httptest2()

httptest2::with_mock_dir("gaz_relations", {
  test_that("relations work", {

    # df
    x <- gaz_search("Bouvet", like = FALSE) %>%
      gaz_relations(type = "administrativepartof", direction = "upper")
    expect_type(x, "list")
    expect_s3_class(x, "mr_df")
    expect_s3_class(x, c("tbl_df", "data.frame"))

  })
})
