#' Get the names for a given MRGID
#'
#' @param mrgid (integer) A valid Marine Regions Gazetteer Identifier ([MRGID])
#'
#' @return a vector with all the names of a Marine Regions Gazetteer entry
#' @export
#' @seealso [gaz_rest], [MRGID]
#'
#' @examples
#' gaz_rest_names_by_mrgid(3293)
#' gaz_rest_names_by_mrgid(14)
gaz_rest_names_by_mrgid <- function(mrgid){

  # Assertions
  mrgid <- checkmate::assert_integerish(mrgid, lower = 1, any.missing = FALSE,
                                       null.ok = TRUE, coerce = TRUE, len = 1)

  # Config
  url <- glue::glue("https://marineregions.org/rest/getGazetteerNamesByMRGID.json/{mrgid}/")

  # perform
  resp <- httr2::request(url) %>%
    httr2::req_user_agent(mr_user_agent) %>%
    httr2::req_headers(accept = "application/json") %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  # Add more info to error message if 404 not found
  if(httr2::resp_status(resp) == 404){
    httr2::resp_check_status(resp, c(
      "x" = glue::glue("The MRGID <{mrgid}> does not exist.")
    ), call = FALSE)
  }

  # Sanity check
  httr2::resp_check_status(resp)

  # End
  resp %>%
    httr2::resp_body_json() %>%
    unlist()

}

# User agent to send in all HTTP requests of this package
mr_user_agent <- glue::glue("{getOption(\"HTTPUserAgent\")}; mregions2 {packageVersion(\"mregions2\")}")

# Create new s3 class mr_df
new_mr_df <- function(x = data.frame()) {
  stopifnot(is.data.frame(x))
  stopifnot("MRGID" %in% names(x))

  attr(x, "class") <- unique(c("mr_df", class(x)))

  x
}

## Functions to fix issues in R CMD Check

# Namespace in Imports field not imported from: 'memoise'
# memoise is not called inside a function but directly to memoise a function. e.g.
#    .f <- function(){"do something"}
#    f <- memoise::memoise(.f)
# but then, memoise is imported but not used in any function.
# solution: create a dummy function that uses memoise. wont be used
mr_memoise <- function(...) memoise::memoise(...)

# Unexported objects imported by ':::' calls:
# `:::` only allowed when the maintainer of the original and target package is the same person
# If not, the safest workaround is to copy&paste the functions in this package
# See https://stat.ethz.ch/pipermail/r-devel/2013-August/067189.html

# rdflib:::c.rdf
# v0.2.5
c_rdf <- function(...){
  rdfs <- list(...)
  loc <- tempdir()
  rdf <- rdfs[[1]]
  for (i in seq_along(rdfs)) {
    f <- file.path(loc, paste0(i, ".rdf"))
    rdflib::rdf_serialize(rdfs[[i]], f, format = "turtle")
    rdflib::rdf_parse(f, rdf = rdf, format = "turtle")
    file.remove(f)
  }
  unlink(loc)
  rdf
}

assert_internet <- function(){
  if(!curl::has_internet()){
    stop("No internet connection. Please check your network settings and try again.", call. = FALSE)
  }
}

assert_service <- function(url_test){
  resp <- httr2::request(url_test) %>%
    httr2::req_user_agent(mr_user_agent) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform() %>%
    httr2::resp_check_status(c(
      "x" = glue::glue("Connection to <{url_test}> failed")
    ))
  invisible(NULL)
}

assert_only_one_filter <- function(cql_filter, filter){
  both_filters_given <- !is.null(cql_filter) & !is.null(filter)
  if(both_filters_given) stop("You must provide one of `cql_filter` or `filter`, not both.", call. = FALSE)
}
