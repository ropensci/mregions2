#' Get all the place types of the Marine Regions Gazetteer
#'
#' @return a data frame with three columns:
#' - `typeID`: the identifier of the place type in the Marine Regions Gazetteer database.
#' - `type`: the name of the place type.
#' - `description`: if available, the description of the place type.
#' @export
#'
#' @seealso [gaz_rest]
#'
#' @examples \donttest{
#' # This
#' gaz_rest_types()
#'
#' # is the same as
#' gaz_types()
#' }
gaz_rest_types <- function(){
  marineregions.org() %>%
    httr2::request() %>%
    httr2::req_url_path_append("/rest/getGazetteerTypes.json/") %>%
    httr2::req_user_agent(mr_user_agent) %>%
    httr2::req_headers(accept = "application/json") %>%
    httr2::req_perform() %>%
    httr2::resp_body_json() %>%
    dplyr::bind_rows()
}

#' @name gaz_types
#' @inherit gaz_rest_types
#' @export
gaz_types <- memoise::memoise(gaz_rest_types)


#' Retrieve Gazetteer Records by Placetype
#'
#' @param x A [place type][gaz_types()]. Either:
#' - (character) The name of a place type.
#' - (integer) The typeid of a place type.
#' @inheritDotParams gaz_rest_records_by_type
#'
#' @seealso [gaz_types()]
#'
#' @return A data frame with Gazetteer entries
#' @export
#'
#' @examples \donttest{
#' # This
#' gaz_search_by_type("EEZ")
#'
#' # is the same as
#' gaz_search_by_type(70)
#' }
gaz_search_by_type <- function(x, ...){
  UseMethod("gaz_search_by_type")
}

#' @rdname gaz_search_by_type
#' @export
gaz_search_by_type.character <- function(x, ...){
  x <- sort(unique(x))

  lapply(x, gaz_rest_records_by_type, ...) %>%
    dplyr::bind_rows() %>%
    new_mr_df()
}

#' @rdname gaz_search_by_type
#' @export
gaz_search_by_type.numeric <- function(x, ...){
  typeID <- NULL
  x <- assert_typeid(x, coerce = TRUE)

  # There is no REST method to return records by typeid
  # Instead, filter gaz_types() to look up the type as a place name
  x <- gaz_types() %>%
    dplyr::filter(typeID %in% x)

  gaz_search_by_type.character(x$type)

}

#' Retrieve Gazetteer Records by Placetype
#'
#' @param type (character) The placetype from [gaz_rest_types()]
#' @param with_geometry (logical) Add geometries to the result data frame? Default = FALSE
#'
#' @return A data frame with Gazetteer entries
#' @seealso [gaz_rest], [gaz_rest_types()]
#' @export
#'
#' @examples \donttest{
#' gaz_rest_records_by_type("FAO Subdivisions")
#' gaz_rest_records_by_type("EEZ")
#' }
gaz_rest_records_by_type <- function(type, with_geometry = FALSE){
  placetype <- type; rm(type)
  MRGID <- NULL

  # Assertions
  checkmate::assert_character(placetype, len = 1)

  # Config
  placetype <- tolower(placetype)
  placetype <- gsub(" ", "+", placetype, fixed = TRUE)
  placetype <- utils::URLencode(placetype)

  # Reusable http request that overrides automatic error check
  get_records_by_type_at <- function(offset){
    marineregions.org() %>%
      httr2::request() %>%
      httr2::req_url_path_append(glue::glue(
        "/rest/getGazetteerRecordsByType.json/{placetype}/"
      )) %>%
      httr2::req_url_query(offset = offset) %>%
      httr2::req_user_agent(mr_user_agent) %>%
      httr2::req_headers(accept = "application/json") %>%
      httr2::req_error(is_error = function(resp) FALSE) %>%
      httr2::req_perform()
  }

  # First request - will work as placeholder
  offset <- 0
  resp <- get_records_by_type_at(offset)

  # Check status: first offset should be 200
  if(httr2::resp_is_error(resp)){

    if(httr2::resp_status(resp) == 404) assert_placetype(placetype)

    httr2::resp_check_status(resp)

  }else{

  # If all ok, continue with first offset
    http_status <- httr2::resp_status(resp)

    resp <- get_records_by_type_at(offset) %>%
      httr2::resp_body_json() %>%
      dplyr::bind_rows()

    # End if there are no more records
    if(nrow(resp) < 100){
      resp <- resp %>% dplyr::arrange(MRGID)

      if(with_geometry){
        resp <- resp %>% gaz_add_geometry()
      }

      return(resp)
    }

    # Enter infinite loop
    while(TRUE){
      offset <- offset + 100
      resp_n <- get_records_by_type_at(offset)
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
