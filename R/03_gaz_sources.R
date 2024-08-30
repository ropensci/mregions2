#' Retrieve Gazetteer Records by Source
#'
#' @param x source as free text or `sourceID` as integer
#' @inheritDotParams gaz_rest_records_by_source -source
#'
#' @seealso [gaz_sources()]
#'
#' @return A data frame with Gazetteer entries
#' @export
#'
#' @examples \donttest{
#' # Check out all sources
#' gaz_sources()
#'
#' # Look up by source name
#' gaz_search_by_source("Gazetteer of Greenland")
#'
#' # Or query by SourceID
#' gaz_search_by_source(386)
#' }
gaz_search_by_source <- function(x, ...){
  UseMethod("gaz_search_by_source")
}

#' @rdname gaz_search_by_source
#' @export
gaz_search_by_source.character <- function(x, ...){
  lapply(unique(x), gaz_rest_records_by_source, ...) %>%
    dplyr::bind_rows() %>%
    new_mr_df()
}

#' @rdname gaz_search_by_source
#' @export
gaz_search_by_source.numeric <- function(x, ...){
  source <- lapply(unique(x), gaz_rest_source_by_sourceid) %>%
    lapply(`[[`, "source") %>%
    unlist()

  lapply(source, gaz_search_by_source.character, ...) %>%
    dplyr::bind_rows() %>%
    new_mr_df()

}

#' Retrieve Gazetteer Records by Source
#'
#' @param source (character) A source from [gaz_rest_sources()]
#' @param with_geometry (logical) Add geometries to the result data frame? Default = FALSE
#'
#' @return A data frame with Gazetteer entries
#' @export
#'
#' @seealso [gaz_rest]
#'
#' @examples \donttest{
#' gaz_rest_records_by_source("ICES Ecoregions")
#' }
gaz_rest_records_by_source <- function(source, with_geometry = FALSE){

  # Assertions
  checkmate::assert_character(source, len = 1, any.missing = FALSE, all.missing = FALSE)
  checkmate::assert_logical(with_geometry, len = 1)

  # Config
  source_parsed <- gsub(" ", "+", source, fixed = TRUE)
  source_parsed <- trimws(source_parsed, "both")
  source_parsed <- utils::URLencode(source_parsed)

  # Perform
  resp <- marineregions.org() %>%
    httr2::request() %>%
    httr2::req_url_path_append(
      glue::glue("/rest/getGazetteerRecordsBySource.json/{source_parsed}/")
    ) %>%
    httr2::req_user_agent(mr_user_agent) %>%
    httr2::req_headers(accept = "application/json") %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  # If error 404, a wrong source was provided
  if(httr2::resp_is_error(resp)){

    if(httr2::resp_status(resp) == 404){
      list_source <- tolower(gaz_rest_sources()$source)
      is_not_choice <- !(tolower(source) %in% list_source)

      if(is_not_choice){
        cli::cli_abort("{.arg source} must be element of set {.fun gaz_sources}, but is {.val {source}}", call. = FALSE)
      }
    }

    # Else, something else occurred, abort
    httr2::resp_check_status(resp)

  }

  # If all ok, continue
  resp <- resp %>%
    httr2::resp_body_json() %>%
    dplyr::bind_rows()

  # Sanity check
  # This service returns 200 and "[]" when there are no records
  if(nrow(resp) == 0){
    cli::cli_abort(c("!" = 'No records found for {.arg source} = {.val {source}}'))
  }

  if(with_geometry){
    resp <- resp %>% gaz_add_geometry()
  }

  resp
}


#' Get all the Marine Regions sources
#'
#' @return a data frame with three columns:
#' - `sourceID`: the identifier of the source in the Marine Regions Gazetteer database.
#' - `source`: the name of the source.
#' - `sourceURL`: if available, the URL of the source.
#' @export
#'
#' @details
#' gaz_search() is a memoised function from gaz_rest_search(). See [memoise::memoise()].
#'
#' @seealso [gaz_rest], [gaz_search_by_source()], [gaz_rest_records_by_source()], [gaz_rest_source_by_sourceid()]
#'
#' @examples \donttest{
#' # This
#' gaz_rest_sources()
#'
#' # is the same as
#' gaz_sources()
#' }
gaz_rest_sources <- function(){
  sourceID <- NULL

  # Reusable http request that overrides automatic error check
  get_source_at <- function(offset){
    marineregions.org() %>%
      httr2::request() %>%
      httr2::req_url_path_append("/rest/getGazetteerSources.json/") %>%
      httr2::req_url_query(offset = offset) %>%
      httr2::req_user_agent(mr_user_agent) %>%
      httr2::req_headers(accept = "application/json") %>%
      httr2::req_error(is_error = function(resp) FALSE) %>%
      httr2::req_perform()
  }

  # First request - will work as placeholder
  offset <- 0
  resp <- get_source_at(offset)

  # Check status: first offset should be 200
  if(httr2::resp_is_error(resp)){
    httr2::resp_check_status(resp)

  }

  resp <- resp %>%
    httr2::resp_body_json() %>%
    dplyr::bind_rows()

  # Enter infinite loop
  while(TRUE){
    offset <- offset + 100
    resp_n <- get_source_at(offset)
    http_status <- httr2::resp_status(resp_n)

    if(httr2::resp_is_error(resp_n) & http_status != 404){
      # Sanity check
      httr2::resp_check_status(resp_n)
    }

    if(http_status == 404){
      # End of the loop
      resp <- resp %>% dplyr::arrange(sourceID)
      return(resp)
    }


    # If no errors and not 404, continue with pagination
    resp <- resp_n %>%
      httr2::resp_body_json() %>%
      dplyr::bind_rows(resp)
  }

}

#' @name gaz_sources
#' @inherit gaz_rest_sources
#' @export
gaz_sources <- memoise::memoise(gaz_rest_sources)


#' Get the name of a source by providing a sourceID
#'
#' @param sourceid (integer) A valid sourceID
#'
#' @return a named vector with the source name and, if available, the url to the source.
#' @export
#'
#' @seealso [gaz_rest], [gaz_sources()]
#'
#' @examples \donttest{
#' gaz_rest_source_by_sourceid(390)
#' gaz_rest_source_by_sourceid(657)
#' }
gaz_rest_source_by_sourceid <- function(sourceid){

  sourceid <- checkmate::assert_int(sourceid, lower = 1, coerce = TRUE)

  marineregions.org() %>%
    httr2::request() %>%
    httr2::req_url_path_append(glue::glue(
      "/rest/getGazetteerSourceBySourceID.json/{sourceid}/"
    )) %>%
    httr2::req_user_agent(mr_user_agent) %>%
    httr2::req_headers(accept = "application/json") %>%
    httr2::req_perform() %>%
    httr2::resp_body_json() %>%
    unlist()

}

