#' Add geometry to a data frame containing a column named MRGID
#'
#' @param x a data frame obtained via mr_gaz_record(s)_by_*()
#'
#' @return the same data frame with a new column the_geom
#' @export
#' @examples
#' mr_gaz_record_by_mrgid(19518, with_geometry = TRUE)
#'
#' # It's the same as
#' mr_gaz_record_by_mrgid(19518) %>% mr_add_geometry()
#'
#' mr_gaz_records_by_name("Belgium") %>% mr_add_geometry()
mr_add_geometry <- function(x){

  # Assertions
  checkmate::assert_data_frame(x)
  stopifnot("MRGID" %in% names(x))

  # Config - get geometries
  the_geom <- lapply(x$MRGID, mr_gaz_geometries, format = "sf", resp_return_error = TRUE) %>%
    suppressWarnings()

  # Logic to add either bounding box or centroid if there is no geometry available
  for(i in 1:length(the_geom)){
    if("httr2_response" %in% class(the_geom[[i]])){
      if(httr2::resp_status(the_geom[[i]]) == 404){


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

            the_geom[[i]] <- tibble::tibble(mrgid = x[i, ]$MRGID,
                                       the_geom = sf::st_as_sfc(the_geom[[i]])) %>%
              sf::st_as_sf()


          }
        }else{

          centroid_exists <- all(c("latitude" %in% names(x[i, ]),
                                   "longitude" %in% names(x[i, ])))

          if(centroid_exists){
            centroid_is_not_na <- all(c(
              !is.na(x[i, ]$latitude),
              !is.na(x[i, ]$longitude)
            ))

              if(centroid_is_not_na){

                the_geom[[i]] <- tibble::tibble(
                  mrgid = x[i, ]$MRGID,
                  the_geom = sf::st_sfc(
                    sf::st_point(c(x[i, ]$longitude, x[i, ]$latitude)),
                    crs = sf::st_crs(4326)
                  )
                ) %>%
                  sf::st_as_sf()

              }

            }else{
              # If no centroid is available, end prematurely
              msg = c(
                "x" = glue::glue("There is no geometry for the GeoObject '{x[i, ]$preferredGazetteerName}' with MRGID: <{x[i, ]$MRGID}>")
              )

              if(x[i, ]$status == "deleted"){
                msg = c(msg,
                        "i" = "Reason: The GeoObject was deleted.",
                       "i" = glue::glue("The preferred alternative is 'TODO mr_gaz_names_by_mrgid' with MRGID: {x[i, ]$accepted}")
                )}else{
                  msg = c(msg,
                          "i" = "Please contact <info@marineregions.org>."
                  )
                }

              cli::cli_abort(msg)
              invisible(NULL)
            }
          }

        }else{
          httr2::resp_check_status(the_geom[[i]])
          return(invisible(NULL))
      }
    }
  }

  # the_geom[id_rm] <- NULL
  the_geom <- the_geom %>% dplyr::bind_rows()

  out <- x %>%
    dplyr::right_join(the_geom, by = c(
      "MRGID" = "mrgid"
    )) %>% sf::st_as_sf()

  return(out)

}


# x <- mr_gaz_record_by_mrgid(19518) %>%
#   dplyr::bind_rows(mr_gaz_record_by_mrgid(58)) %>%
#   dplyr::bind_rows(mr_gaz_record_by_mrgid(97))
#
# x1 <- x %>% dplyr::bind_rows(mr_gaz_record_by_mrgid(8399))
#
# debug(mr_add_geometry)
#
# mr_add_geometry(x1)

# User agent to send in all HTTP requests of this package
mr_user_agent <- glue::glue("{getOption(\"HTTPUserAgent\")}; mregions2 {packageVersion(\"mregions2\")}")
