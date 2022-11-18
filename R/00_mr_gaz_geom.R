#' Funtions to return the geometry of a marine regions geoobject
#'
#' @param mrgid int. A valid marine regions gazetteer identifier
#' @param sourceid int. A source ID
#' @param attribute_value char. The attribute value that identifies the geoobject
#' @param format The preferred output format. One of:
#' - "sfc": Simple Feature geometry object. See 'sf'
#' - "wkt": Geometry representation as Well-Known Text
#' - "rdf": Geometry as an object of class 'rdf". See 'rdflib'
#'
#' Default is "sfc"
#'
#' @return the geometry of a marine regions geoobject
#' @export
#'
#' @examples
#' mr_gaz_geometry(mrgid = 3293, sourceid = 79, attribute_value = 3293)
#' mr_gaz_geometries(mrgid = 58, format = "wkt")
mr_gaz_geometry <- function(mrgid, sourceid, attribute_value, format = "sfc", ...){

  # Assertions
  checkmate::assert_int(mrgid, lower = 1)
  checkmate::assert_int(sourceid, lower = 1)

  # Config
  url = glue::glue("https://marineregions.org/rest/getGazetteerGeometry.ttl/{mrgid}/?source={sourceid}&attributeValue={attribute_value}")

  # perform
  geom_perform(url, format, multipart = FALSE, mrgid = mrgid, ...)

}

#' @rdname mr_gaz_geometry
mr_gaz_geometries <- function(mrgid, format = "sfc", multipart = TRUE, ...){

  # Assertions
  checkmate::assert_int(mrgid, lower = 1)

  # Config
  url = glue::glue("https://marineregions.org/rest/getGazetteerGeometries.ttl/{mrgid}/")

  # Perform
  geom_perform(url, format, multipart, mrgid = mrgid, ...)

}

# Performs the aquisition of geometry
geom_perform <- function(url, format, multipart = TRUE, mrgid, resp_return_error = FALSE){
  # Assert format
  checkmate::assert_choice(format, c("sfc", "sf", "wkt", "rdf"))

  # Perform

  # Request
  geom <- httr2::request(url) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_user_agent("mregions2") %>%
    httr2::req_headers(accept = "text/turtle") %>%
    httr2::req_perform()

  if(resp_return_error & httr2::resp_status(geom) == 404){
      return(geom)
  }

  geom <- geom %>%
    httr2::resp_check_status() %>%
    httr2::resp_body_string() %>%
    rdflib::rdf_parse(format = "turtle")

  # Premature end for rdf
  if(format == "rdf"){
    return(geom)
  }

  # Extract geometry
  geom <- geom %>%
    rdflib::rdf_query(
      "PREFIX gsp: <http://www.opengis.net/ont/geosparql#>

     SELECT ?s ?the_geom
     WHERE {
        ?s gsp:asWKT ?the_geom .
     }"
    ) %>% suppressMessages()

  geom$the_geom <- gsub("<","", geom$the_geom, fixed = TRUE)
  geom$the_geom <- strsplit(geom$the_geom, ">")[[1]][2]
  geom$the_geom <- trimws(geom$the_geom)

  # Extract sourceid, transform to sf and group the geometry by sourceid
  geom <- geom %>%
    dplyr::mutate(
      s = lapply(s, httr2::url_parse) %>%
        lapply(`[[`, c('query')) %>%
        lapply(`[[`, c('source')) %>%
        unlist()
    ) %>% sf::st_as_sf(
      wkt = "the_geom",
      crs = 4326
    )

  # Raise warning if there are several sources
  n_sources <- length(unique(geom$s))
  if(n_sources > 1 & multipart){
    # sources <- geom$s %>% lapply(mr_gaz_source_by_sourceid) %>% unlist() %>% paste0(collapse = ", ")
    # msg <- glue::glue("Argument 'multipart = TRUE' ignored because there are {n_sources} sources for the geoobject {mr_gaz_name_by_mrgid(mrgid)} with MRGID = {mrgid}.")
    msg = "Argument 'multipart = TRUE' ignored because there is more than one source"
    warning(msg, call. = FALSE)
  }else if(n_sources == 1 & multipart){
    geom <- geom %>%
      dplyr::group_by(s) %>%
      dplyr::summarise(the_geom = sf::st_combine(the_geom)) %>%
      dplyr::ungroup()
  }

  if(format == "wkt"){
    wkt <- sf::st_as_text(geom)
    return(wkt)
  }

  if(format == "sf"){
    geom$s <- NULL
    geom$mrgid <- mrgid
    return(geom)
  }

  if(format == "sfc"){
    sfc <- sf::st_as_sfc(geom)
    return(sfc)
  }

}
