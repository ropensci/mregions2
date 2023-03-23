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

# sf:::aggregate.sf
# v1.0.7
aggregate_sf <- function (x, by, FUN, ..., do_union = TRUE, simplify = TRUE,
          join = sf::st_intersects)
{
  if (inherits(by, "sf") || inherits(by, "sfc")) {
    if (inherits(by, "sfc"))
      by <- sf::st_sf(by)
    i <- join(sf::st_geometry(by), sf::st_geometry(x))
    sf::st_geometry(x) <- NULL
    a <- stats::aggregate(x[unlist(i), , drop = FALSE], list(rep(seq_len(nrow(by)),
                                                         lengths(i))), FUN, ...)
    nrow_diff <- nrow(by) - nrow(a)
    if (nrow_diff > 0) {
      a_na <- a[rep(NA, nrow(by)), ]
      a_na[a$Group.1, ] <- a
      a <- a_na
    }
    a$Group.1 <- NULL
    row.names(a) <- row.names(by)
    sf::st_set_geometry(a, sf::st_geometry(by))
  }
  else {
    crs <- sf::st_crs(x)
    lst <- lapply(split(sf::st_geometry(x), by), function(y) do.call(c,
                                                                    y))
    geom <- do.call(sf::st_sfc, lst[!sapply(lst, is.null)])
    if (do_union)
      geom <- sf::st_union(sf::st_set_precision(geom, sf::st_precision(x)),
                      by_feature = TRUE)
    sf::st_geometry(x) <- NULL
    x <- stats::aggregate(x, by, FUN, ..., simplify = simplify)
    sf::st_geometry(x) <- geom
    sf::st_crs(x) <- crs
    geoms <- which(vapply(x, function(vr) inherits(vr, "sfc"),
                         TRUE))
    agr_names <- names(x)[-geoms]
    agr <- rep("aggregate", length(agr_names))
    names(agr) <- agr_names
    n <- if (!is.null(names(by)))
      names(by)
    else paste0("Group.", seq_along(by))
    agr[n] <- "identity"
    sf::st_agr(x) <- agr
    x
  }
}

assert_internet <- function(){
  if(!curl::has_internet()){
    stop("No internet connection. Please check your network settings and try again.", call. = FALSE)
  }
}

.assert_service <- function(url_test){
  resp <- httr::GET(url_test, httr::add_headers(`User-Agent` = mr_user_agent))
  resp$content <- NULL

  if(httr::http_error(resp)){
    cli::cli_abort(c(
      "x" = "Connection to {.url {url_test}} failed",
      "i" = "Reason: {.val {httr::http_status(resp)$message}}"
    ), call = NULL)
  }

  invisible(NULL)
}
assert_service <- memoise::memoise(.assert_service)

