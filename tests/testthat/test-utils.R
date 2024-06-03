test_that("user agent", {

  expect_match(mr_user_agent, "R")
  expect_match(mr_user_agent, "mregions2")

})

test_that("new_mr_df works", {
  expect_error(new_mr_df(1))
  expect_error(new_mr_df(list()))
  expect_error(new_mr_df("not a data frame"))
  expect_error(new_mr_df(data.frame()))

  df <- data.frame(MRGID = 3293)
  df_mr <- new_mr_df(df)
  expect_s3_class(df_mr, "mr_df")

})

test_that("mr_memoise works", {

  .f <- function() invisible(NULL)
  expect_false(memoise::is.memoised(.f))

  f <- mr_memoise(.f)
  expect_true(memoise::is.memoised(f))
})

test_that("not internet msg works", {
  withr::local_envvar("TESTPKG.NOINTERNET" = "blop")
  expect_error(assert_internet(), "No internet connection")
})

test_that("Server status 500 handled", {
  mock_500 <- function(req) {
    httr2::response(status_code = 500)
  }

  .f <- function(){
    httr2::with_mocked_responses(
      mock_500,
      assert_service("")
    )}

  expect_error(.f(), regexp = "500")
})


test_that("assert_service returns NULL",{
  mock_200 <- function(req) {
    httr2::response(status_code = 200)
  }

  .f <- function(){
    httr2::with_mocked_responses(
      mock_200,
      assert_service("")
    )
  }

  expect_invisible(.f())
  expect_null(.f())
})


test_that("Only one filter assertion works",{

  .f <- function() assert_only_one_filter(NULL, NULL)
  expect_null(.f())

  .f <- function() assert_only_one_filter("", NULL)
  expect_null(.f())

  .f <- function() assert_only_one_filter(NULL, "")
  expect_null(.f())

  .f <- function() assert_only_one_filter("", "")
  expect_error(.f())
})

test_that("Imported method rdflib:::c.rdf as c_rdf works", {
  pepe <- rdflib::rdf() %>%
    rdflib::rdf_add("person", "hasName", "pepe")
  juan <- rdflib::rdf() %>%
    rdflib::rdf_add("person", "hasName", "juan")

  people <- rdflib::rdf()
  people <- c_rdf(people, pepe, juan)

  expect_s3_class(people, "rdf")
  expect_length(people, 2)
})

test_that("check status methods work", {
  df <- data.frame(
    MRGID = 1,
    preferredGazetteerName = "Name",
    preferredGazetteerNameLang = "Language",
    status = "deleted",
    accepted = 2,
    stringsAsFactors = TRUE
  ) %>% new_mr_df()


  stop_if_deleted(df) %>%
    expect_error("DELETED", fixed = TRUE)

  df$status <- "altclass"
  warn_if_altclass(df) %>%
    expect_warning("ALTERNATIVE CLASSIFICATION", fixed = TRUE)

})

test_that("assert typeid works", {
  test_fail <- c(9999, 10, 14, 9998)
  .f <- function() assert_typeid(test_fail)
  expect_error(.f(), 'are "9998" and "9999"', fixed = TRUE)

  test_ok <- assert_typeid(c(14, 14, 10), coerce = TRUE)
  expect_type(test_ok, "integer")
  expect_identical(test_ok, c(10L, 14L))
})

test_that("assert placetype works", {
  .f <- function() assert_placetype(c("foo2", "EEZ", "foo"))
  expect_error(.f(), 'are "foo" and "foo2"', fixed = TRUE)

  .f <- function() assert_placetype("eez")
  expect_error(.f(), 'capital', fixed = TRUE)

  .f <- function() assert_placetype(c("EEZ", "FAO fishing area"))
  expect_invisible(.f())
  expect_null(.f())
})


