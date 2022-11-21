#' Get the names for a given MRGID
#'
#' @param mrgid An existing Marine Regions Gazetteer Identifier
#'
#' @return a vector with all the names of a Marine Regions Gazetteer GeoObject
#' @export
#'
#' @examples
#' mr_gaz_names_by_mrgid(3293)
#' mr_gaz_names_by_mrgid(14)
mr_gaz_names_by_mrgid <- function(mrgid){

  # Assertions
  mrgid = checkmate::assert_integerish(mrgid, lower = 1, any.missing = FALSE,
                                       null.ok = TRUE, coerce = TRUE, len = 1)

  # Config
  url = glue::glue("https://marineregions.org/rest/getGazetteerNamesByMRGID.json/{mrgid}/")

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
