# This package needs both httr and httr2:
# uses httr2 for the gazetteer
# uses httr for the data products via a dependency on ows4r
# To avoid conflicts by masked functions, better to load the libraries in the tests

# clean up httptest2 or httptest to not mask functions
load_httptest <- function(version){
  suppressWarnings({
    if("httptest2" %in% (.packages())){
      httptest2::set_redactor(NULL)
      detach("package:httptest2", unload=TRUE, force=TRUE)
    }
    library(httptest, warn.conflicts = FALSE)
    options(httptest2.verbose =TRUE)
  })

}

load_httptest2 <- function(version){
  suppressWarnings({
    if("httptest" %in% (.packages())){
      httptest::set_redactor(NULL)
      httptest::set_requester(NULL)
      detach("package:httptest", unload=TRUE, force=TRUE)
    }
    library(httptest2, warn.conflicts = FALSE)
    options(httptest.verbose=TRUE)
  })
}
