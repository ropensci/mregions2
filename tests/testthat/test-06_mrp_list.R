use_httptest2()

httptest2::with_mock_dir("mrp_list", {
  test_that("mrp_list() works", {
    skip_if_offline()

    x <- mrp_list()
    expect_type(x, "list")
    expect_s3_class(x, c("tbl_df", "data.frame"))
    expect_gt(nrow(x), 1)

  })
})
