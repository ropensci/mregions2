# httptest2::with_mock_dir("prod/fail/", {
  test_that("mrp_get: Bad filters errors surfaced", {
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
# })

# httptest2::with_mock_dir("prod/ok/", {
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
    # withr::local_options("mregions2.download_path" = "./prod/ok/geo")

    expect_sf <- function(x){
      expect_type(x, "list")
      expect_s3_class(x, c("sf"))
      expect_s3_class(x, c("data.frame", "tbl_df"))
      expect_gte(nrow(x), 1)
    }


    # Check without caching
    withr::local_options("TESTPKG.CACHETIME" = 0)

    # Mexican ECS Deposit
    .f1 <- function() mrp_get("ecs", cql_filter = "mrgid = 64123")
    expect_sf(.f1())
    expect_s3_class(sf::st_geometry(.f1()), "sfc_POLYGON")

    # Mexican ECS Deposit line
    .f2 <- function() mrp_get("ecs_boundaries", cql_filter = "line_id = 4232")
    expect_sf(.f2())
    expect_s3_class(sf::st_geometry(.f2()), "sfc_LINESTRING")

    # Check with caching
    # withr::local_options("TESTPKG.CACHETIME" = Inf)
    #
    # expect_message(.f1(), "Cache", fixed = TRUE)
    # expect_sf(.f1())
    # expect_s3_class(sf::st_geometry(.f1()), "sfc_POLYGON")
    #
    # expect_message(.f2(), "Cache", fixed = TRUE)
    # expect_sf(.f2())
    # expect_s3_class(sf::st_geometry(.f2()), "sfc_LINESTRING")
  })

# }, simplify = TRUE)


