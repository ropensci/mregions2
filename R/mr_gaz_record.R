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
mr_gaz_ldes <- function(mrgid, type = "list"){

  req <- httr2::request("http://marineregions.org") %>%
    httr2::req_url_path_append("mrgid") %>%
    httr2::req_url_path_append(mrgid) %>%
    httr2::req_headers(accept = "application/ld+json") %>%
    req_mr_user_agent()

  # Build and perform
  resp <- req %>%
    httr2::req_perform()

  if(type == "rdf"){
    out <- resp %>%
      httr2::resp_body_string() %>%
      rdflib::rdf_parse("jsonld")
  }
  if(type == "list"){
    out <- resp %>%
      httr2::resp_body_json()
  }

  out

  # TODO: add assertions
}


#' Get the geometry a Marine Regions Gazetteer record as Well Known Text
#'
#' @param mrgid Marine Regions Gazetter Unique Identifier
#'
#' @return A string with the geometry of the Marine Regions object as WKT
#' @export
#'
#' @examples
#' test <- mr_gaz_geometry(4280)
#' test <- mr_gaz_geometry(3293)
#' test <- mr_gaz_geometry(26567)
mr_gaz_geometry <- function(mrgid){
  feed <- mr_gaz_ldes(mrgid, "list")

  has_geometry <- "mr:hasGeometry" %in% names(feed)

  if(has_geometry){

    req_geom <- function(url){
      req <- httr2::request(url)
      req <- req %>%
        req_mr_user_agent() %>%
        httr2::req_headers(accept = "application/ld+json") %>%
        httr2::req_perform() %>%
        httr2::resp_body_json(encoding = "UTF-8")

      req <- req$`mr:hasGeometry`$`gsp:asWKT`

      # TODO: read CRS on the fly
      req <- gsub("<http://www.opengis.net/def/crs/OGC/1.3/CRS84> ", "", req, fixed = TRUE)
    }

    geom <- lapply(feed$`mr:hasGeometry`, req_geom) %>%
      unlist() %>%
      sf::st_as_sfc(crs = 4326) %>%
      sf::st_combine()

    return(geom)
  }

}


#' Get a Marine Regions Gazetteer record
#'
#' @param mrgid Marine Regions Gazetter Unique Identifier
#' @param add_geometry set FALSE to not retrieve the geometry
#'
#' @return A Marine Regions Gazetteer record
#' @export
#'
#' @examples
#' test <- mr_gaz_record(3293)
#' test <- mr_gaz_record(4280)
mr_gaz_record <- function(mrgid, add_geometry = TRUE){
  feed <- mr_gaz_ldes(mrgid)

  req <- httr2::request(feed$`@id`)
  req <- req %>%
    req_mr_user_agent() %>%
    httr2::req_headers(accept = "application/json") %>%
    httr2::req_perform() %>%
    httr2::resp_body_json(encoding = "UTF-8")

  for(i in 1:length(req)){
    if(req[[i]] %>% is.null()){
      req[[i]] <- NA
    }
  }

  # TODO: outsource function mr_null_to_na() (the for loop) into utils

  req <- req %>%
    tibble::as_tibble()

  if(add_geometry){
    req$geom <- mr_gaz_geometry(mrgid)[1, ]
    req <- sf::st_as_sf(req)
  }

  req
}
