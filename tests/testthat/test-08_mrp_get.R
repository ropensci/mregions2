httptest::with_mock_dir("mrp_get", {
  test_that("mrp_get() works", {

    # Returns a sf object
    x <- mrp_get("eez", cql_filter = "mrgid = 5688")
    expect_type(x, "list")
    expect_s3_class(x, c("sf"))
    expect_s3_class(x, c("data.frame", "tbl_df"))
    expect_gte(nrow(x), 1)


    # Transformation in multipolygon or multilinestring works
    expect_s3_class(sf::st_geometry(x), "sfc_MULTIPOLYGON")

    x <- mrp_get("eez_boundaries", cql_filter = "line_id = 3690")
    expect_type(x, "list")
    expect_s3_class(x, c("sf"))
    expect_s3_class(x, c("data.frame", "tbl_df"))
    expect_gte(nrow(x), 1)
    expect_s3_class(sf::st_geometry(x), "sfc_MULTILINESTRING")

    # Expect errors
    .f <- function() mrp_get("this product does not exists")
    expect_error(.f())

    .f <- function() mrp_get("eez_boundaries", cql_filter = "this is not a good filter")
    expect_error(.f())

  })
})

httptest::with_mock_dir("mrp_init_wfs_client", {
  test_that("mrp_init_wfs_client() works", {

    # Returns a wfs client
    x <- mrp_init_wfs_client()
    expect_type(x, "environment")
    expect_s3_class(x, c("WFSClient", "OWSClient", "OGCAbstractObject", "R6" ))
    expect_match(x$getUrl(), "geo.vliz.be")

    # Expect errors
    .f <- function() mrp_init_wfs_client(version = 2.0)
    expect_error(.f())

    .f <- function() mrp_init_wfs_client(version = "0.0.0")
    expect_error(.f())

  })
})

httptest::with_mock_dir("mrp_colnames", {
  test_that("mrp_colnames() works", {

    # Returns a data frame
    x <- mrp_colnames("ecs_boundaries")
    expect_s3_class(x, c("data.frame"))
    expect_gte(nrow(x), 1)
    expect_gt(ncol(x), 1)

    # Check all columns are of type character
    invisible(apply(x, 2, expect_vector, ptype = character()))

    # Expect errors
    .f <- function() mrp_colnames("this is not a data product")
    expect_error(.f())

    .f <- function() mrp_colnames(c("ecs", "eez"))
    expect_error(.f())

    .f <- function() mrp_colnames(1)
    expect_error(.f())
  })
})

httptest::with_mock_dir("mrp_col_unique", {
  test_that("mrp_col_unique() works", {

    # two names for the same function
    expect_true(all.equal(mrp_col_unique, mrp_col_distinct))

    # Returns a vector of type character
    x <- mrp_col_unique("ecs_boundaries", "line_type")
    expect_vector(x, ptype = character())
    expect_gte(length(x), 1)

    # Returns a vector of type numeric
    x <- mrp_col_unique("ecs", "mrgid")
    expect_vector(x, ptype = numeric())
    expect_gte(length(x), 1)

    # Returns a vector of type date
    x <- mrp_col_unique("eez_boundaries", "doc_date")
    expect_vector(x)
    expect_s3_class(x, "Date")
    expect_gte(length(x), 1)

    # Expect errors
    .f <- function() mrp_col_unique("this is not a data product", "mrgid")
    expect_error(.f())

    .f <- function() mrp_col_unique(1, "mrgid")
    expect_error(.f())

    .f <- function() mrp_col_unique("ecs", 1)
    expect_error(.f())

    .f <- function() mrp_col_unique("high_seas", "the_geom")
    expect_error(.f())
  })
})
