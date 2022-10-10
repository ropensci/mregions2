


new_MRGeoObject <- function(x = list()){
  structure(x, class = "MRGeoObject")
}

#' Retrieves a Marine Regions Gazetteer record as Open Linked Data
#'
#' @param mrgid Marine Regions Gazetter Unique Identifier
#' @param type Return resuts as RDF or as list
#'
#' @return A Marine Regions Gazetteer record as triples in RDF or list
#' @export
#'
#' @examples
#' test <- mr_gaz_ldes(26567)
#' test <- mr_gaz_ldes(3293, type = "list")
#' test <- mr_gaz_ldes(1902, type = "rdf")
#'
#'
#'

mr_get <- function(mrgid, ...){
  UseMethod("mr_get")
}


# Give mrgids, return the geoobject
# mr_get(3293)
mr_get.default <- function(mrgid, ...){

  # Request
  mr_get_n <- function(x){
    req <- httr2::request("http://marineregions.org") %>%
      httr2::req_url_path_append("mrgid") %>%
      httr2::req_url_path_append(x) %>%
      httr2::req_headers(accept = "application/ld+json") %>%
      req_mr_user_agent()

    resp <- req %>%
      httr2::req_perform() %>%
      httr2::resp_body_json()

    resp
  }

  resp <- lapply(mrgid, mr_get_n)

  resp <- new_MRGeoObject(resp)

  names(resp) <- as.character(mrgid)

  resp
}

# Print nice info about the geoobject
print.MRGeoObject <- function(x, ...){

  if(class(x)!="MRGeoObject") stop("The argument Object must be a class MRGeoObject")


  cat(crayon::grey(glue::glue('A Marine Regions Geo Object: {length(x)}')))

  # cat(message("this is a geoobject"))
  x <- unlist(x)
  x <- subset(x, grepl("@context", names(x)))
  x <- new_mr_geo_object(x)

  print.simple.list(x)
}

library(magrittr)
x <- mr_get(3293)

x[["3293"]][["skos:prefLabel"]][[1]]$`@value`

#> # A tibble: 1 × 1
#>       x
#>   <dbl>
#> 1  123.

test <- mr_get(14)
test1 <- print(test)

test1 <- unlist(test)

print.simple.list(test)





