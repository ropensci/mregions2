#' Get the geometries of a Marine Regions Geo-Object
#'
#' @param x object to retrieve the geometries from. Accepted:
#' * (integer) A valid Marine Regions Gazetteer Identifier ([MRGID])
#' * A data frame retrieved with [mregions2] via its functions [gaz_search()],
#' [gaz_search_by_source()], [gaz_search_by_type()] or [gaz_relations()]. See details.
#'
#' @details
#' You can pass the output of most `gaz_*` functions to `gaz_geometry()` to retrieve the
#' geometry the gazetteer entry. The data frame is then transformed into a [sf::sf] object.
#'
#' ## Developer info
#' This is done in the method [gaz_geometry.mr_df()]. `mr_df` is a class defined in
#' this package to ensure the data frame passed to gaz_geometry has a variable with [MRGID].
#'
#' @export
#'
#' @return A sfc object (default), a sf data frame, a WKT string or an RDF object
#'
#' @examples \donttest{
#' gaz_geometry(3293)
#' gaz_geometry(3293, format = "wkt")
#' gaz_geometry(3293, format = "rdf")
#'
#' gaz_search(3293) |> gaz_geometry()
#' }
gaz_geometry <- function(x, ...){
  UseMethod("gaz_geometry")
}

#' @name gaz_geometry
#'
#' @inheritDotParams gaz_rest_geometries -mrgid
#'
#' @export
gaz_geometry.numeric <- function(x, ...){

  x <- lapply(x, gaz_rest_geometries, ...)

  format <- substitute(list(...))$format
  if(is.null(format)) format <- "sfc"

  if(format == "sfc"){
    out <- unlist(x, recursive = FALSE, use.names = FALSE) %>%
      sf::st_sfc(crs = 4326)
    return(out)
  }
  if(format == "sf") return( x %>% dplyr::bind_rows() )
  if(format == "wkt") return( unlist(x) )
  if(format == "rdf"){

    if(length(x) == 1) return( x[[1]] )
    if(length(x) > 1){
      out <- x[[1]]
      for(i in 2:length(x)){
        out <- c_rdf(out, x[[i]])
      }
      return(out)
    }
  }

}

#' @name gaz_geometry
#'
#' @export
gaz_geometry.mr_df <- function(x, ...){
  wrapr::stop_if_dot_args(substitute(list(...)), "Must not provide aditional arguments")
  x %>% gaz_add_geometry()
}

#' Get the geometries associated with a gazetteer record
#'
#' @param mrgid (integer) A valid Marine Regions Gazetteer Identifier ([MRGID])
#' @param format (character) The preferred output format. One of:
#' - "sfc": Simple Feature geometry object. See 'sf'
#' - "wkt": Geometry representation as [Well-Known Text](https://en.wikipedia.org/wiki/Well-known_text)
#' - "rdf": Geometry as an object of class 'rdf". See 'rdflib'
#'
#' Default is "sfc"
#' @param multipart (logical) Some Geo-Objects are compound of more than one part.
#' - If FALSE, returns singlepart geometries (e.g. POLYGON, LINESTRING)
#' - If TRUE (default), returns multipart geometries (e.g. MULTIPOLYGON, MULTILINESTRING)
#'
#' @param ... reserved for internal use
#'
#' @seealso [gaz_rest]
#'
#' @return A sfc object (default), a sf data frame, a WKT string
#' or an RDF object
#'
#' @examples \donttest{
#' gaz_rest_geometries(3293)
#' gaz_rest_geometries(3293, format = "wkt")
#' gaz_rest_geometries(3293, format = "rdf")
#' }
#' @export
gaz_rest_geometries <- function(mrgid, format = "sfc", multipart = TRUE, ...){

  # Assertions
  checkmate::assert_int(mrgid, lower = 1)

  # Config
  url <- glue::glue('{marineregions.org()}/rest/getGazetteerGeometries.ttl/{mrgid}/')

  # Perform
  geom_perform(url, format, multipart, mrgid = mrgid, ...)

}

#' Get one single geometry associated with a gazetteer record
#'
#' This function is mainly for internal use. Please use
#' [gaz_rest_geometries()] instead.
#'
#' @inheritParams gaz_rest_geometries
#' @param sourceid A source ID
#' @param attribute_value The attribute value that identifies the Geo-Object
#'
#' @noRd
gaz_rest_geometry <- function(mrgid, sourceid, attribute_value,
                              format = "sfc", ...){

  # Assertions
  checkmate::assert_int(mrgid, lower = 1)
  checkmate::assert_int(sourceid, lower = 1)

  # Config
  url <- paste0(marineregions.org(), "/rest/getGazetteerGeometry.ttl/", mrgid,
    "/?source=", sourceid,
    "&attributeValue=", attribute_value
  )

  # perform
  geom_perform(url, format, multipart = FALSE, mrgid = mrgid, ...)

}


#' Performs the aquisition of geometry
#'
#' @inheritParams gaz_rest_geometry
#' @param resp_return_error the response should raise an error
#' if HTTP Status 3xx, 4xx or 5xxx. Reserved for internal use
#' @param multipart return multipart?
#'
#' @noRd
geom_perform <- function(url, format, multipart = TRUE, mrgid,
                         resp_return_error = FALSE){
  s <- the_geom <- NULL

  # Assert format
  checkmate::assert_choice(format, c("sfc", "sf", "wkt", "rdf"))

  # Perform

  # Request
  geom <- httr2::request(url) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_user_agent(mr_user_agent) %>%
    httr2::req_headers(accept = "text/turtle") %>%
    httr2::req_perform()

  if(httr2::resp_status(geom) == 404){
    df <- gaz_rest_record_by_mrgid(mrgid)
    stop_if_deleted(df)

    # Pass to gaz_add_geometry
    if(resp_return_error) return(geom)
  }

  geom <- geom %>%
    httr2::resp_check_status() %>%
    httr2::resp_body_string() %>%
    rdflib::rdf_parse(format = "turtle")

  # Premature end for rdf
  if(format == "rdf"){ return(geom) }

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

  geom$the_geom <- strsplit(geom$the_geom, ">") %>%
    lapply(`[`, n = 2) %>%
    unlist() %>%
    trimws()

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
    # TODO: add more info about sources.
    # Currently not possible - need further web services
    # sources <- geom$s %>% lapply(mr_gaz_source_by_sourceid) %>%
    # unlist() %>% paste0(collapse = ", ")
    # msg <- glue::glue("Argument 'multipart = TRUE' ignored because there
    # are {n_sources} sources for the geoobject {mr_gaz_name_by_mrgid(mrgid)}
    # with MRGID = {mrgid}.")
    msg <- paste0("Argument 'multipart = TRUE' ignored because",
                  "there is more than one source"
    )
    warning(msg, call. = FALSE)
  }else if(n_sources == 1 & multipart){
    geom <- geom %>%
      dplyr::group_by(s) %>%
      dplyr::summarise(the_geom = sf::st_combine(the_geom)) %>%
      dplyr::ungroup()
  }

  if(format == "wkt"){
    wkt <- sf::st_as_text(geom$the_geom)
    return(wkt)
  }

  if(format == "sf"){
    geom <- geom %>%
      dplyr::transmute(
        MRGID = as.integer(mrgid), the_geom
      )
    return(geom)
  }

  if(format == "sfc"){
    sfc <- sf::st_as_sfc(geom)
    return(sfc)
  }

}


#' Add geometry to a mr_df
#'
#' @param x an object of class mr_df
#'
#' @noRd
gaz_add_geometry <- function(x){

  # Assertions
  checkmate::assert_data_frame(x, min.rows = 1)

  # Config - get geometries
  the_geom <- lapply(x$MRGID, gaz_rest_geometries,
                     format = "sf", resp_return_error = TRUE) %>%
    suppressWarnings()

  # Logic to add either bounding box or centroid if there is no
  # geometry available
  for(i in seq_along(the_geom)){
    if("httr2_response" %in% class(the_geom[[i]])){
      if(httr2::resp_status(the_geom[[i]]) == 404){

        # Try to get BBOX
        bbox_exists <- all(c("minLatitude" %in% names(x[i, ]),
                             "minLongitude" %in% names(x[i, ]),
                             "maxLatitude" %in% names(x[i, ]),
                             "maxLongitude" %in% names(x[i, ])
        ))

        if(bbox_exists){
          bbox_is_not_na <- all(c(!is.na(x[i, ]$minLatitude),
                                  !is.na(x[i, ]$maxLatitude),
                                  !is.na(x[i, ]$minLongitude),
                                  !is.na(x[i, ]$maxLongitude)))

          if(bbox_is_not_na){
            the_geom[[i]] <- sf::st_bbox(c(xmin = x[i, ]$minLongitude,
                                           xmax = x[i, ]$maxLongitude,
                                           ymax = x[i, ]$maxLatitude,
                                           ymin = x[i, ]$minLatitude),
                                         crs = sf::st_crs(4326))

            the_geom[[i]] <- data.frame(
              MRGID = x[i, ]$MRGID,
              the_geom = sf::st_as_sfc(the_geom[[i]]),
              stringsAsFactors = FALSE
            )

            attr(the_geom[[i]], "class") <- c("tbl_df", "tbl", "data.frame")

            the_geom[[i]] <- sf::st_as_sf(the_geom[[i]])

          }
        }

        # Try to get centroid
        no_bbox <- "httr2_response" %in% class(the_geom[[i]])
        if(no_bbox){
          centroid_exists <- all(c("latitude" %in% names(x[i, ]),
                                   "longitude" %in% names(x[i, ])))

          if(centroid_exists){
            centroid_is_not_na <- all(c(
              !is.na(x[i, ]$latitude),
              !is.na(x[i, ]$longitude)
            ))

            if(centroid_is_not_na){

              the_geom[[i]] <- data.frame(
                MRGID = x[i, ]$MRGID,
                the_geom = sf::st_sfc(
                  sf::st_point(c(x[i, ]$longitude, x[i, ]$latitude)),
                  crs = sf::st_crs(4326)
                ), stringsAsFactors = FALSE
              )

              attr(the_geom[[i]], "class") <- c("tbl_df", "tbl", "data.frame")

              the_geom[[i]] <- sf::st_as_sf(the_geom[[i]])

            }
          }
        }

        # No geometry: raise error
        no_geometry <- "httr2_response" %in% class(the_geom[[i]])
        if(no_geometry){
          msg <- glue::glue(
            'The geometry of "{.val {x[i, ]$preferredGazetteerName}}" <{x[i, ]$MRGID}> is not available.'
          )
          stop(msg)
        }
      }else{
        # If other http status, raise error
        httr2::resp_check_status(the_geom[[i]], info = glue::glue(
          "At method: {the_geom[[1]]$method} {the_geom[[i]]$url}"
        ))
        return(invisible(NULL))
      }
    }
  }

  y <- dplyr::bind_rows(the_geom)

  out <- dplyr::right_join(x, y, by = "MRGID") %>%
    sf::st_as_sf()

  return(out)

}
