# This package needs both httr and httr2:
# uses httr2 for the gazetteer
# uses httr for the data products via a dependency on ows4r
# To avoid conflicts by masked functions, better to load the libraries in the tests

# clean up httptest2 or httptest to not mask functions
use_httptest <- function(version){
  suppressWarnings({
    if("httptest2" %in% (.packages())){
      detach("package:httptest2", unload=TRUE)
    }
    library(httptest, warn.conflicts = FALSE)
  })
}

use_httptest2 <- function(version){
  suppressWarnings({
    if("httptest" %in% (.packages())){
      detach("package:httptest", unload=TRUE)
    }
    library(httptest2, warn.conflicts = FALSE)
  })
}

options(httptest2.verbose =TRUE)
# options(httptest.mock.paths = "tests")
