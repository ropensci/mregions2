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

  # Perform
  resp <- marineregions.org() %>%
    httr2::request() %>%
    httr2::req_url_path_append(glue::glue(
      "/rest/getGazetteerNamesByMRGID.json/{mrgid}/"
    )) %>%
    httr2::req_user_agent(mr_user_agent) %>%
    httr2::req_headers(accept = "application/json") %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  # Add more info to error message if 404 not found
  if(httr2::resp_status(resp) == 404){
    httr2::resp_check_status(resp, c(
      "x" = glue::glue("The MRGID <{mrgid}> does not exist.")
    ))
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

is_internet_down <- function(){
  if(nzchar(Sys.getenv("TESTPKG.NOINTERNET"))){
    return(TRUE)
  }

  !curl::has_internet()
}

assert_internet <- function(){
  if(is_internet_down()){
    stop("No internet connection. Please check your network and try again.", call. = FALSE)
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

stop_if_deleted <- function(mr_df){
  for(i in seq_along(nrow(mr_df))){
    is_deleted <- mr_df[i, ]$status == "deleted"
    if(is_deleted){
      msg <- glue::glue(
        'The Geo-Object "{mr_df[i, ]$preferredGazetteerName}" <{mr_df[i, ]$MRGID}> has status DELETED. Use <{mr_df[i, ]$accepted}> instead.'
      )
      stop(msg, call. = FALSE)
    }
  }
  invisible(NULL)
}

warn_if_altclass <- function(mr_df){
  for(i in seq_along(nrow(mr_df))){
    is_altclass <- mr_df[i, ]$status == "altclass"
    if(is_altclass){
      msg <- glue::glue(
        'The Geo-Object "{mr_df[i, ]$preferredGazetteerName}" <{mr_df[i, ]$MRGID}> has status ALTERNATIVE CLASSIFICATION. Consider using <{mr_df[i, ]$accepted}> instead.'
      )
      warning(msg, call. = FALSE)
    }
  }
  invisible(NULL)
}

assert_typeid <- function(typeid = NULL, coerce = FALSE){
  if(!is.null(typeid)){
    typeid <- checkmate::assert_integerish(typeid, lower = 1, upper = 999999999, min.len = 1,
                                            any.missing = FALSE, all.missing = FALSE,
                                            coerce = TRUE)
    typeid <- sort(unique(typeid))

    is_not_typeid <- !(all(typeid %in% gaz_types()$typeID))
    if(is_not_typeid){
      typeid_not_part_of <- as.character(
        subset(typeid, !(typeid %in% gaz_types()$typeID))
      )
      cli::cli_abort("{.arg typeid} must be element of set {.fun gaz_types}, but {?is/are} {.val {typeid_not_part_of}}", call. = FALSE)
    }
  }

  if(coerce){
    return(typeid)
  }
}

assert_placetype <- function(type = NULL){
  if(!is.null(type)){
    is_not_type <- !(all(type %in% gaz_types()$type))
    if(is_not_type){
      type_not_part_of <- sort(subset(type, !(type %in% gaz_types()$type)))

      msg <- c("{.arg type} must be element of set {.fun gaz_types}, but {?is/are} {.val {type_not_part_of}}")

      issue_is_capital_letters <- all(tolower(type) %in% tolower(gaz_types()$type))
      if(issue_is_capital_letters){
        msg <- c(msg, "i" = "Note match is sensitive to capital letters.")
      }

      cli::cli_abort(msg, call. = FALSE)
    }
  }
  invisible(NULL)
}

#' URL marine regions website
#'
#' @details
#'
#' Reads marine regions url from options: marineregions.url
#' Use for debugging with dev site
#'
#' @noRd
marineregions.org <- function(){
  getOption("marineregions.url", "https://marineregions.org/")
}

assert_mrgid_exists <- function(mrgid){
  gaz_rest_names_by_mrgid(mrgid)
  invisible(NULL)
}

is_test <- function(){
  nzchar(Sys.getenv("TESTPKG.ISTEST"))
}
