#' Get all the placetypes of the Marine Regions Gazetteer
#'
#' @return a tibble with the type and definition if available
#' @export
#'
#' @examples
#' types <- gaz_rest_types()
#' # Same as
#' types <- gaz_types()
gaz_rest_types <- function(){
  url <- "https://marineregions.org/rest/getGazetteerTypes.json/"

  resp <- httr2::request(url) %>%
    httr2::req_user_agent("mregions2") %>%
    httr2::req_headers(accept = "application/json") %>%
    httr2::req_perform() %>%
    httr2::resp_body_json() %>%
    dplyr::bind_rows()

  resp
}

#' @rdname gaz_rest_types
#' @export
gaz_types <- memoise::memoise(gaz_rest_types)


#' @rdname gaz_rest_records_by_type
#' @export
gaz_search_by_type <- function(x, ...){
  lapply(x, gaz_rest_records_by_type, ...) %>%
    dplyr::bind_rows()
}

#' Retrieve Gazetteer Records by Placetype
#'
#' @param type The placetype from gaz_rest_types()
#' @param with_geometry Logical. Add geometries to the result data frame? Default = FALSE
#'
#' @return A tibble with all Gazetteer records of the specified placetype.
#' @export
#'
#' @examples
#' gaz_rest_records_by_type("FAO Subdivisions")
#' gaz_rest_records_by_type("EEZ")
gaz_rest_records_by_type <- function(placetype, with_geometry = FALSE){

  # Assertions
  checkmate::assert_character(placetype)

  # Config
  placetype <- tolower(placetype)
  placetype <- gsub(" ", "+", placetype, fixed = TRUE)
  placetype <- utils::URLencode(placetype)

  # Reusable http request that overrides automatic error check
  get_source <- function(offset){
    url = glue::glue("https://marineregions.org/rest/getGazetteerRecordsByType.json/{placetype}/?offset={offset}")

    resp <- httr2::request(url) %>%
      httr2::req_user_agent(mr_user_agent) %>%
      httr2::req_headers(accept = "application/json") %>%
      httr2::req_error(is_error = function(resp) FALSE) %>%
      httr2::req_perform()
  }

  # First request - will work as placeholder
  offset = 0
  resp <- get_source(offset)

  # Check status: first offset should be 200
  if(httr2::resp_is_error(resp)){

    # If first is 404, the placetype must not be correct. Assert.
    if(httr2::resp_status(resp) == 404){

      list_placetypes <- tolower(gaz_rest_types()$type)

      is_not_choice <- !(placetype %in% list_placetypes)

      if(is_not_choice){
        stop(glue::glue("`placetype` must be element of set `gaz_rest_types()`, but is '{placetype}'"), call. = FALSE)
      }
    }

    # In any other case of error, abort and return the HTTP error
    httr2::resp_check_status(resp)

  }else{

  # If all ok, continue with first offset

    http_status <- httr2::resp_status(resp)
    resp <- get_source(offset) %>%
      httr2::resp_body_json() %>%
      dplyr::bind_rows()

    # Enter infinite loop
    while(TRUE){
      offset = offset + 100
      resp_n <- get_source(offset)
      http_status <- httr2::resp_status(resp_n)

      if(httr2::resp_is_error(resp_n) & http_status != 404){
        # Sanity check
        httr2::resp_check_status(resp_n)
      }

      if(http_status == 404){
        # End of the loop
        resp <- resp %>% dplyr::arrange(MRGID)

        if(with_geometry){
          resp <- resp %>% gaz_add_geometry()
        }

        return(resp)
      }


      # If no errors and not 404, continue with pagination
      resp <- resp_n %>%
        httr2::resp_body_json() %>%
        dplyr::bind_rows(resp)

    }
  }

}
