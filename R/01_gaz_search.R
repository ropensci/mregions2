#' Search in the Marine Regions Gazetteer by names, MRGID or reverse geocode with a pair of
#' WGS84 coordinates x and y
#'
#' @param x object to perform the search with. Can be:
#'   * (character) Free text search
#'   * (integer) A valid Marine Regions Gazetteer Identifier ([MRGID])
#'   * (double) Longitude in WGS84
#'   * Aditionally, you can pass objects of class [sf::sf] or [sf::sfc] with geometry
#'     of class `POINT`
#' @param y (double) Latitude in WGS84 (Optional)
#' @inheritDotParams gaz_rest_record_by_mrgid -mrgid
#' @inheritDotParams gaz_rest_records_by_name -name
#' @inheritDotParams gaz_rest_records_by_names -names
#' @inheritDotParams gaz_rest_records_by_lat_long -latitude -longitude
#'
#' @return A data frame with Gazetteer entries
#' @export
#'
#' @examples \donttest{
#'
#' # Look-up a name in the Gazetteer
#' gaz_search("North Sea")
#'
#' # Get the entries of two known MRGID including their geometry
#' gaz_search(c(14, 17), with_geometry = TRUE)
#'
#' # Maybe the name is in another language...
#' gaz_search("Noordzee", language = "nl")
#'
#' # Get all the records intersecting with the longitude 51.21551 and latitude 2.927
#' # restricting to some placetypes
#' gaz_search(x = 2.927, y = 51.21551, typeid = c(255, 259))
#' }
gaz_search <- function(x, ...){
  UseMethod("gaz_search")
}

#' @export
#' @rdname gaz_search
gaz_search.character <- function(x, ...){

  is_plural <- length(x) > 1

  if(is_plural) out <- gaz_rest_records_by_names(x, ...)
  if(!is_plural) out <- gaz_rest_records_by_name(x, ...)

  new_mr_df(out)
}

#' @export
#' @rdname gaz_search
gaz_search.numeric <- function(x, ..., y = NULL){

  # If latitude is provided - we are doing a geospatial query
  if(!is.null(y)){
    checkmate::assert_numeric(x, lower = -180, upper = 180, len = 1)
    checkmate::assert_numeric(y, lower = -90, upper = 90, len = 1)

    x <- sf::st_point(c(x, y), dim = "XY")
    out <- gaz_search.sfg(x, ...)
  }

  # If not, expected that x is an mrgid
  if(is.null(y)){
    x <- lapply(x, gaz_rest_record_by_mrgid, ...)

    # Logic to return an RDF document
    if(methods::hasArg("rdf")){
      if(sys.call()$rdf){
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

    out <- dplyr::bind_rows(x)
  }

  new_mr_df(out)
}



#' @export
#' @rdname gaz_search
gaz_search.sfg <- function(x, ...){
  checkmate::assert_class(x, "POINT")

  x <- sf::st_sfc(x)

  gaz_search.sfc(x, ...)

}

#' @export
#' @rdname gaz_search
gaz_search.sf <- function(x, ...){
  checkmate::assert_data_frame(x, nrows = 1)

  x <- sf::st_geometry(x)

  gaz_search.sfc(x, ...)
}

#' @export
#' @rdname gaz_search
gaz_search.sfc <- function(x, ...){
  checkmate::assert_class(x, "sfc_POINT")

  coords <- sf::st_coordinates(x)
  longitude <- coords[, 1]
  latitude <- coords[, 2]

  out <- gaz_rest_records_by_lat_long(latitude, longitude, ...)

  new_mr_df(out)
}


#' Get one record for the given MRGID
#'
#' @param mrgid (integer) A valid Marine Regions Gazetteer Identifier ([MRGID])
#' @param with_geometry (logical) Add geometry to the result data frame? Default = FALSE
#' @param rdf (logical) Return an object of class [rdflib::rdf]?
#'
#' @export
#'
#' @seealso [gaz_rest], [MRGID]
#'
#' @return A data frame with the Gazetteer entry
#'
#' @examples \donttest{
#' gaz_rest_record_by_mrgid(3293)
#' gaz_rest_record_by_mrgid(3293, with_geometry = TRUE)
#' gaz_rest_record_by_mrgid(3293, rdf = TRUE)
#' }
gaz_rest_record_by_mrgid <- function(mrgid, with_geometry = FALSE, rdf = FALSE){

  # Assertions
  mrgid <- checkmate::assert_integerish(mrgid, lower = 1, any.missing = FALSE,
                                        null.ok = TRUE, coerce = TRUE, len = 1)

  # Config
  content_type <- ifelse(rdf, "text/turtle", "application/json")

  # perform
  resp <- marineregions.org() %>%
    httr2::request() %>%
    httr2::req_url_path_append(glue::glue("/mrgid/{mrgid}")) %>%
    httr2::req_user_agent(mr_user_agent) %>%
    httr2::req_headers(accept = content_type) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  # Add more info to error message if 404 not found
  if(httr2::resp_status(resp) == 404){
    httr2::resp_check_status(resp, c(
      "i" = glue::glue("The MRGID <{mrgid}> does not exist.")
    ))
  }

  # Sanity check
  httr2::resp_check_status(resp)

  # Premature end if rdf
  if(rdf){

    if(with_geometry){
      cli::cli_warn(
        c("!" = "Argument {.arg with_geometry = TRUE} ignored when {.arg rdf = TRUE}.",
          "i" = "Request geometries with {.fun gaz_geometry}")
      )
    }

    out <- resp %>%
      httr2::resp_body_string() %>%
      rdflib::rdf_parse("turtle")

    return(out)
  }

  # If json
  out <- resp %>%
    httr2::resp_body_json() %>%
    dplyr::bind_rows()

  if(with_geometry){
    out <- out %>% gaz_add_geometry()
  }

  return(out)

}


#' Get Gazetteer Records for a given name
#'
#' @param name (character) Term to search in the Marine Regions Gazetteer
#' @param with_geometry (logical) Add geometry to the result data frame? Default = FALSE
#' @param typeid (numeric) Restrict to one or more placetypeIDs. Retrieve a list of placetypeIDs with [gaz_rest_types()]
#' @param language (character) Restrict to one language. Provide as a 2 digits ISO-639. See [ISOcodes::ISO_639_2].
#' @param like (logical) Add a '%'-sign before and after the name? (SQL LIKE function). Default = TRUE
#' @param fuzzy (logical) Use Levenshtein query to find nearest matches? Default = TRUE
#'
#' @export
#' @seealso [gaz_rest], [gaz_rest_records_by_name]
#' @return A data frame with Gazetteer entries
#'
#' @examples \donttest{
#' gaz_rest_records_by_name("Belgian Exclusive Economic Zone", with_geometry = TRUE)
#' gaz_rest_records_by_name("Bélgica", language = "es")
#' gaz_rest_records_by_name("Belgium", typeid = c(350, 351))
#' }
gaz_rest_records_by_name <- function(name, with_geometry = FALSE, typeid = NULL, language = NULL, like = TRUE, fuzzy = TRUE){
  MRGID <- NULL

  checkmate::assert_logical(with_geometry, len = 1)
  checkmate::assert_character(name, len = 1)
  typeid <- checkmate::assert_integerish(typeid, lower = 1, any.missing = FALSE,
                                        null.ok = TRUE, coerce = TRUE)
  checkmate::assert_character(language, len = 1, null.ok = TRUE, n.chars = 2)

  # Assert language choice
  if(!is.null(language)){
    not_iso_639_2 <- !(language %in% unique(ISOcodes::ISO_639_2$Alpha_2))
    if(not_iso_639_2){
      cli::cli_abort(c(
        "!" = "{.arg language} must be a valid ISO-632 language code of two digits.",
        "i" = "Run {.var ISOcodes::ISO_639_2} to see the allowed language ISO codes."
      ))
    }
  }

  checkmate::assert_logical(like, len = 1)
  like_url <- like %>% as.character() %>% tolower()

  checkmate::assert_logical(fuzzy, len = 1)
  fuzzy_url <- fuzzy %>% as.character() %>% tolower()

  # Config
  name <- gsub(" ", "+", name, fixed = TRUE) %>% utils::URLencode()

  # Reusable http request that overrides automatic error check
  gaz_records_by_name_at <- function(offset){
    req <- marineregions.org() %>%
      httr2::request() %>%
      httr2::req_url_path_append(
        glue::glue("/rest/getGazetteerRecordsByName.json/{name}/")
      ) %>%
      httr2::req_url_query(like = like_url, fuzzy = fuzzy_url, language = language,
                           typeID = paste0(as.character(typeid), collapse = ","),
                           offset = offset, count = 100) %>%
      httr2::req_user_agent(mr_user_agent) %>%
      httr2::req_headers(accept = "application/json") %>%
      httr2::req_error(is_error = function(resp) FALSE) %>%
      httr2::req_perform()
  }

  # First request - will work as placeholder
  offset <- 0
  resp <- gaz_records_by_name_at(offset)

  # Check status: first offset should be 200
  if(httr2::resp_is_error(resp)){

    # If first is 404, either the typeID is not correct or the name does not exist
    if(httr2::resp_status(resp) == 404){

      # base msg
      msg <- c(
        "x" = glue::glue("There are no matches for '{name}'")
      )

      if(!is.null(typeid)){
        assert_typeid(typeid)

        msg <- c(msg,
                 "i" = glue::glue("The term '{name}' may not be available for the selected type."),
                 "*" = "Try with `typeid = NULL`."
        )
        httr2::resp_check_status(resp, info = msg)
      }


      # name is not correct
      if(!fuzzy) msg <- c(msg, "i" = "Did you set `fuzzy` = TRUE?")
      if(!like) msg <- c(msg, "i" = "Did you set `like` = TRUE?")

      if(!is.null(language)){
        lan <- subset(ISOcodes::ISO_639_2$Name,
                      ISOcodes::ISO_639_2$Alpha_2 == language)[1]
        lan <- strsplit(lan, split = ";")[[1]][1]
        msg <- c(msg,
                 "i" = glue::glue("The {lan} term '{name}' may not be available."),
                 "*" = "Try with `language = NULL`."
        )
      }
      if(fuzzy & like & is.null(language)){
        msg <- c(msg, "i" = "Did you check for typos?")
      }

      httr2::resp_check_status(resp, info = msg)
    }

    # In any other case of error, abort and return the HTTP error
    httr2::resp_check_status(resp)

  }

  # If all ok, continue with first offset
  resp <- resp %>%
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
    resp_n <- gaz_records_by_name_at(offset)
    http_status <- httr2::resp_status(resp_n)

    if(httr2::resp_is_error(resp_n) & http_status != 404){
      # Sanity check
      httr2::resp_check_status(resp_n)
    }

    if(http_status == 404){
      # End of the loop
      resp <- resp %>% dplyr::arrange(MRGID)

      if(with_geometry) resp <- resp %>% gaz_add_geometry()

      return(resp)
    }


    # If no errors and not 404, continue with pagination
    resp <- resp_n %>%
      httr2::resp_body_json() %>%
      dplyr::bind_rows(resp)
  }

}



#' Get Gazetteer Records for all given names
#'
#' @param names (character) Vector with the terms to search in the Marine Regions Gazetteer
#' @param with_geometry (logical) Add geometry to the result data frame? Default = FALSE
#' @param like (logical) Add a '%'-sign before and after the name? (SQL LIKE function). Default = TRUE
#' @param fuzzy (logical) Use Levenshtein query to find nearest matches? Default = TRUE
#'
#' @export
#' @seealso [gaz_rest], [gaz_rest_records_by_name]
#' @return A data frame with Gazetteer entries
#'
#' @export
#'
#' @examples \donttest{
#' gaz_rest_records_by_names(
#'   c("Belgian Exclusive Economic Zone", "Dutch Exclusive Economic Zone")
#' )
#' }
gaz_rest_records_by_names <- function(names, with_geometry = FALSE, like = TRUE, fuzzy = TRUE){

  # Assertions
  checkmate::assert_logical(with_geometry, len = 1)
  checkmate::assert_character(names, min.len = 1, unique = TRUE, any.missing = FALSE, all.missing = FALSE)
  checkmate::assert_logical(like, len = 1)
  checkmate::assert_logical(fuzzy, len = 1)


  # Config
  names_url <- gsub(" ", "+", names, fixed = TRUE)
  names_url <- trimws(names_url, "both")
  names_url <- paste0(names_url, collapse = "/")
  names_url <- utils::URLencode(names_url)

  fuzzy_url <- fuzzy %>% as.character() %>% tolower()
  like_url <- like %>% as.character() %>% tolower()


  url <- glue::glue("https://marineregions.org")

  # Perform
  out <- marineregions.org() %>%
    httr2::request() %>%
    httr2::req_url_path_append(
      glue::glue("/rest/getGazetteerRecordsByNames.json/{like_url}/{fuzzy_url}/{names_url}/")
    ) %>%
    httr2::req_user_agent(mr_user_agent) %>%
    httr2::req_headers(accept = "application/json") %>%
    httr2::req_perform() %>%
    httr2::resp_body_json() %>%
    dplyr::bind_rows()

  if(with_geometry) out <- out %>% gaz_add_geometry()

  out
}


#' Get all gazetteer records where the geometry intersects with the given latitude and longitude
#'
#' @param latitude (double) A decimal number which ranges from -90 to 90. Coordinates are assumed to be in WGS84
#' @param longitude (double) A decimal number which ranges from -180 to 180. Coordinates are assumed to be in WGS84
#' @param with_geometry (logical) Add geometries to the result data frame? Default = FALSE
#' @param typeid (numeric) Restrict to one or more placetypeIDs. Retrieve a list of placetypeIDs with [gaz_rest_types()]
#'
#' @export
#' @seealso [gaz_rest]
#'
#' @return A data frame with Gazetteer entries
#'
#' @examples \donttest{
#' gaz_rest_records_by_lat_long(51.21551, 2.927)
#' gaz_rest_records_by_lat_long(51.21551, 2.927,
#'                              with_geometry = TRUE,
#'                              typeid = c(255, 259))
#' }
gaz_rest_records_by_lat_long <- function(latitude, longitude, with_geometry = FALSE, typeid = NULL){
  MRGID <- NULL

  # Assertions
  checkmate::assert_logical(with_geometry, len = 1)
  checkmate::assert_double(latitude, lower = -90, upper = 90, len = 1)
  checkmate::assert_double(longitude, lower = -180, upper = 180, len = 1)
  typeid <- checkmate::assert_integerish(typeid, lower = 1, any.missing = FALSE,
                                        null.ok = TRUE, coerce = TRUE)

  # Config
  url <- glue::glue("https://marineregions.org")

  # Reusable http request that overrides automatic error check
  get_records_by_lat_long_at <- function(offset){
    req <- marineregions.org() %>%
      httr2::request() %>%
      httr2::req_url_path_append(
        glue::glue("/rest/getGazetteerRecordsByLatLong.json/{latitude}/{longitude}/")
      ) %>%
      httr2::req_user_agent(mr_user_agent) %>%
      httr2::req_headers(accept = "application/json") %>%
      httr2::req_url_query(typeID = paste0(as.character(typeid), collapse = ","),
                           offset = offset) %>%
      httr2::req_error(is_error = function(resp) FALSE) %>%
      httr2::req_perform()
  }

  # First request - will work as placeholder
  offset <- 0
  resp <- get_records_by_lat_long_at(offset)

  # Check status: first offset should be 200
  if(httr2::resp_is_error(resp)){

    # If first is 404, either the typeID is not correct or the name does not exist
    if(httr2::resp_status(resp) == 404){

      # base msg
      msg <- c(
        "x" = glue::glue("There are no matches for point with latitude: '{latitude}' and longitude: '{longitude}'")
      )

      if(!is.null(typeid)){
        assert_typeid(typeid)

        msg <- c(msg,
                 "i" = "There might not be matches for the selected type.",
                 "*" = "Try with `typeid` = NULL."
        )
        httr2::resp_check_status(resp, info = msg)
      }

      httr2::resp_check_status(resp, info = msg)
    }

    # In any other case of error, abort and return the HTTP error
    httr2::resp_check_status(resp)

  }
  # If all ok, continue with first offset
  resp <- resp %>%
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

  # If 100 rows or more, enter infinite loop
  while(TRUE){

    offset <- offset + 100
    resp_n <- get_records_by_lat_long_at(offset)
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





