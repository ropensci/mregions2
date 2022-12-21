#' Retrieve Gazetteer Records by Source
#'
#' @param x source as free text or `sourceID` as integer
#'
#' @seealso [gaz_sources()]
#'
#' @return A data frame with Gazetteer entries
#' @export
#'
#' @examples
#' # Check out all sources
#' gaz_sources()
#'
#' # Look up by source name and include the geometries
#' contiguous_zone_src = "Flanders Marine Institute (2019). Maritime Boundaries Geodatabase: Contiguous Zones (24NM), version 3. Available online at http://www.marineregions.org/. https://doi.org/10.14284/384"
#' gaz_search_by_source(contiguous_zone_src, with_geometry = TRUE)
#'
#' # Or pass the geometries later
#' gaz_search_by_source(contiguous_zone_src) %>% gaz_geometry()
#'
#' # Or query by SourceID all the maritime boundaries
#' gaz_search_by_source(630:634)
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
  source = purrr::map_df(unique(x), ~gaz_rest_source_by_sourceid(.x))

  lapply(source["source"], gaz_search_by_source.character, ...) %>%
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
#' @examples
#' gaz_rest_records_by_source("ICES Ecoregions")
gaz_rest_records_by_source <- function(source, with_geometry = FALSE){

  # Assertions
  checkmate::assert_character(source, len = 1, any.missing = FALSE, all.missing = FALSE)
  checkmate::assert_logical(with_geometry, len = 1)

  # Config
  source_parsed <- gsub(" ", "+", source, fixed = TRUE)
  source_parsed <- trimws(source_parsed, "both")
  url <- glue::glue("https://marineregions.org/rest/getGazetteerRecordsBySource.json/{source_parsed}/")
  url <- utils::URLencode(url)

  # Perform
  resp <- httr2::request(url) %>%
    httr2::req_user_agent(mr_user_agent) %>%
    httr2::req_headers(accept = "application/json") %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  # If error 404, either a wrong source was provided or there are no records for that source.
  if(httr2::resp_is_error(resp)){

    if(httr2::resp_status(resp) == 404){
      list_source <- tolower(gaz_rest_sources()$source)
      is_not_choice <- !(tolower(source) %in% list_source)

      if(is_not_choice){
        stop(glue::glue("`source` must be element of set `gaz_sources()`, but is '{source}'"), call. = FALSE)
      }

    }

    # Else, something else occurred, abort
    httr2::resp_check_status(resp)

  }

  # If all ok, continue
  resp <- resp %>%
    httr2::resp_body_json() %>%
    dplyr::bind_rows()

  if(nrow(resp) == 0){
    cli::cli_abort(c(
      "!" = "There are no Geo-Objects for this source.",
      "i" = "Source: {.val {source}}"
    ))
  }

  if(with_geometry){
    resp <- resp %>% gaz_add_geometry()
  }

  resp
}
# src <- "Van Eck, B.T.M. (Ed.) (1999). De Scheldeatlas: een beeld van een estuarium. Rijksinstituut voor Kust en Zee/Schelde InformatieCentrum: Middelburg. ISBN 90-369-3434-6. 120 pp."
# gaz_rest_records_by_source(src)
# gaz_rest_records_by_source("this is not a source")


#' Get all the Marine Regions sources
#'
#' @return a data frame with three columns:
#' - `sourceID`: the identifier of the source in the Marine Regions Gazetteer database.
#' - `source`: the name of the source.
#' - `sourceURL`: if available, the URL of the source.
#' @export
#'
#' @seealso [gaz_rest], [gaz_search_by_source()], [gaz_rest_records_by_source()], [gaz_rest_source_by_sourceid()]
#'
#' @examples
#' # This
#' gaz_rest_sources()
#'
#' # is the same as
#' gaz_sources()
gaz_rest_sources <- function(){

  # Reusable http request that overrides automatic error check
  get_source <- function(offset){
    url = glue::glue("https://www.marineregions.org/rest/getGazetteerSources.json/?offset={offset}")

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
    httr2::resp_check_status(resp)

  }else{

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
        resp <- resp %>% dplyr::arrange(sourceID)
        return(resp)
      }


      # If no errors and not 404, continue with pagination
      resp <- resp_n %>%
        httr2::resp_body_json() %>%
        dplyr::bind_rows(resp)
    }
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
#' @examples
#' gaz_rest_source_by_sourceid(390)
#' gaz_rest_source_by_sourceid(657)
gaz_rest_source_by_sourceid <- function(sourceid){

  sourceid <- checkmate::assert_int(sourceid, lower = 1, coerce = TRUE)

  url = glue::glue("https://www.marineregions.org/rest/getGazetteerSourceBySourceID.json/{sourceid}/")

  resp <- httr2::request(url) %>%
    httr2::req_user_agent("mregions2") %>%
    httr2::req_headers(accept = "application/json") %>%
    httr2::req_perform() %>%
    httr2::resp_body_json() %>%
    unlist()

  resp

}


