#' Search in the gazetteer by names, MRGID or get all the Geo-Objects
#'   that intersect with a pair of WGS84 coordinates x and y
#'
#' @param x object to perform the search with. Can be:
#'   * Free text search
#'   * Valid marine regions gazetteer identifiers as integer
#'   * Longitude in WGS84
#' @param y Optional: Latitude in WGS84.
#' @param ... params to be passed to the REST methods
#'
#' @return A data frame with the found Geo-Objects
#' @export
#'
#' @examples
#' # Get the geo-objects of two known MRGID
#' gaz_search(c(14, 17))
#'
#' # Or Look-up a name in the Gazetteer
#' gaz_search("Belgian Part of the North Sea")
#'
#' # Maybe the name is in another language...
#' gaz_search("Belgie", language = "nl", fuzzy = TRUE)
#' gaz_search("Bélgica", language = "es", fuzzy = TRUE)
#'
#' # Get all the records intersecting with the longitude 51.21551
#'   and latitude 2.927
#' gaz_search(x = 51.21551, y = 2.927)
#'
gaz_search <- function(x, ...){
  UseMethod("gaz_search")
}

#' @export
#' @rdname gaz_search
gaz_search.character <- function(x, ...){

  is_plural <- length(x) > 1

  if(is_plural){
    gaz_rest_records_by_names(x, ...)
  }

  if(!is_plural){
    gaz_rest_records_by_name(x, ...)
  }

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
    x <- lapply(x, gaz_rest_record_by_mrgid, rdf = FALSE, ...)
    out <- dplyr::bind_rows(x)
  }

  out
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
  longitude = coords[, 1]
  latitude = coords[, 2]

  gaz_rest_records_by_lat_long(latitude, longitude, ...)
}


#' Get one record for the given MRGID
#'
#' @param mrgid An existing Marine Regions Gazetteer Identifier
#' @param with_geometry Logical. Add geometry to the result data frame? Default = FALSE
#' @param rdf Logical. Return an object of class "rdf". See package 'rdflib'
#'
#' @export
#'
#' @examples
#' # Get the Belgian Part of the North Sea
#' gaz_rest_record_by_mrgid(3293)
#'
#' # Better with its geometry
#' geo <- gaz_rest_record_by_mrgid(3293, with_geometry = TRUE)
#' mapview::mapview(gaz)
#'
#' # As an RDF document
#' gaz_rest_record_by_mrgid(3293, rdf = TRUE)
gaz_rest_record_by_mrgid <- function(mrgid, with_geometry = FALSE, rdf = FALSE){

  # Assertions
  mrgid = checkmate::assert_integerish(mrgid, lower = 1, any.missing = FALSE,
                                        null.ok = TRUE, coerce = TRUE, len = 1)

  # Config
  url <- glue::glue("https://marineregions.org/mrgid/{mrgid}")
  content_type <- ifelse(rdf, "text/turtle", "application/json")

  # perform
  resp <- httr2::request(url) %>%
    httr2::req_user_agent(mr_user_agent) %>%
    httr2::req_headers(accept = content_type) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  # Add more info to error message if 404 not found
  if(httr2::resp_status(resp) == 404){
    httr2::resp_check_status(resp, c(
      "x" = glue::glue("The MRGID {.val {mrgid}} does not exist.")
    ))
  }

  # Sanity check
  httr2::resp_check_status(resp)

  # Premature end if rdf
  if(rdf){
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
#' @param name Term to search in the Marine Regions Gazetteer
#' @param with_geometries Logical. Add geometries to the result data frame? Default = FALSE
#' @param typeid Restrict to one or more placetypeIDs. See function gaz_rest_types() to retrieve a list of placetypeIDs. Default = NULL
#' @param language Restrict to one language. Provide as a 2 digits ISO-639. Default = NULL
#' @param like Logical. Add a '%'-sign before and after the name? (SQL LIKE function). Default = TRUE
#' @param fuzzy Logical. Use Levenshtein query to find nearest matches? Default = FALSE
#'
#' @export
#'
#' @examples
#' gaz_rest_records_by_name("Belgian Exclusive Economic Zone", with_geometry = TRUE)
#' gaz_rest_records_by_name("Bélgica", language = "es")
#' gaz_rest_records_by_name("Belgica", language = "es", fuzzy = TRUE)
#' gaz_rest_records_by_name("Belgium", typeid = c(350, 351))
gaz_rest_records_by_name <- function(name, with_geometries = FALSE, typeid = NULL, language = NULL, like = TRUE, fuzzy = FALSE){

  # Assert name
  checkmate::assert_character(name, len = 1)

  # Assert typeid
  typeid = checkmate::assert_integerish(typeid, lower = 1, any.missing = FALSE,
                                        null.ok = TRUE, coerce = TRUE)

  # Assert language
  checkmate::assert_character(language, len = 1, null.ok = TRUE)

  if(!is.null(language)){
    not_iso_639_2 <- !(language %in% unique(ISOcodes::ISO_639_2$Alpha_2))
    if(not_iso_639_2){
      cli::cli_abort(c(
        "x" = "{.var language} must be a valid ISO-632 language code of two digits:",
        "i" = "Run {.var ISOcodes::ISO_639_2} to see the allowed language ISO codes."
      ))
    }
  }

  # Assert logicals
  checkmate::assert_logical(like, len = 1)
  like_url = like %>% as.character() %>% tolower()

  checkmate::assert_logical(fuzzy, len = 1)
  fuzzy_url = fuzzy %>% as.character() %>% tolower()

  # Config
  name <- gsub(" ", "+", name, fixed = TRUE)
  url <- glue::glue("https://marineregions.org/rest/getGazetteerRecordsByName.json/{name}/")
  url <- utils::URLencode(url)

  # Reusable http request that overrides automatic error check
  get_source <- function(offset){
    req <- httr2::request(url) %>%
      httr2::req_user_agent(mr_user_agent) %>%
      httr2::req_headers(accept = "application/json") %>%
      httr2::req_url_query(like = like_url, fuzzy = fuzzy_url, language = language,
                           typeID = paste0(as.character(typeid), collapse = ","),
                           offset = offset, count = 100) %>%
      httr2::req_error(is_error = function(resp) FALSE) %>%
      httr2::req_perform()
  }

  # First request - will work as placeholder
  offset = 0
  resp <- get_source(offset)

  # Check status: first offset should be 200
  if(httr2::resp_is_error(resp)){

    # If first is 404, either the typeID is not correct or the name does not exists
    if(httr2::resp_status(resp) == 404){

      # base msg
      msg <- c(
        "x" = glue::glue("There are no matches for '{name}'")
      )

      if(!is.null(typeid)){
        is_not_typeid <- !(all(typeid %in% gaz_rest_types()$typeID))
        if(is_not_typeid){
          stop(glue::glue("`typeid` must be element of set `gaz_rest_types()`, but is '{typeid}'"), call. = FALSE)
        }

        msg <- c(msg,
                 "i" = glue::glue("The term '{name}' may not be available for the selected type."),
                 "*" = "Try with `typeid` = NULL."
                 )
        httr2::resp_check_status(resp, info = msg)
      }


      # name is not correct
      if(!fuzzy){
        msg <- c(msg, "i" = "Did you set `fuzzy` = TRUE?")
      }
      if(!like){
        msg <- c(msg, "i" = "Did you set `like` = TRUE?")
      }
      if(!is.null(language)){
        lan <- strsplit(subset(ISOcodes::ISO_639_2$Name, ISOcodes::ISO_639_2$Alpha_2 == language)[1], split = ";")[[1]][1]
        msg <- c(msg,
                 "i" = glue::glue("The {lan} term '{name}' may not be available."),
                 "*" = "Try with `language` = NULL."
        )
      }
      if(fuzzy & like & is.null(language)){
        msg <- c(msg, "i" = "Did you check for misspelling mistakes?")
      }

      httr2::resp_check_status(resp, info = msg)
    }

    # In any other case of error, abort and return the HTTP error
    httr2::resp_check_status(resp)

  }else{

    # If all ok, continue with first offset
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

        if(with_geometries){
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



#' Get Gazetteer Records for all given names
#'
#' @description
#' Retrieve all Records of the [Marine Gazetteer](https://marineregions.org/gazetteer.php) that contain one or more parameters in `names` in their `preferredGazetteerName`.
#'
#' @param names Terms to search in the Marine Regions Gazetteer given as a vector
#' @param with_geometries Logical. Add geometries to the result data frame? Default = FALSE
#' @param like Logical. Add a '%'-sign before and after the name? (SQL LIKE function). Default = TRUE
#' @param fuzzy Logical. Use Levenshtein query to find nearest matches? Default = FALSE
#'
#' @export
#'
#' @examples
#' eez <- c("Belgian Exclusive Economic Zone", "Dutch Exclusive Economic Zone")
#' gaz_rest_records_by_names(eez, with_geometries = TRUE)
gaz_rest_records_by_names <- function(names, with_geometries = FALSE, like = TRUE, fuzzy = FALSE){

  # Assertions
  checkmate::assert_character(names, min.len = 1, unique = TRUE, any.missing = FALSE, all.missing = FALSE)
  checkmate::assert_logical(like, len = 1)
  checkmate::assert_logical(fuzzy, len = 1)


  # Config
  names_url <- gsub(" ", "+", names, fixed = TRUE)
  names_url <- trimws(names_url, "both")
  names_url <- paste0(names_url, collapse = "/")
  names_url <- utils::URLencode(names_url)
  fuzzy_url = fuzzy %>% as.character() %>% tolower()
  like_url = like %>% as.character() %>% tolower()
  url <- glue::glue("https://marineregions.org/rest/getGazetteerRecordsByNames.json/{like_url}/{fuzzy_url}/{names_url}/")

  # Perform
  out <- httr2::request(url) %>%
    httr2::req_user_agent(mr_user_agent) %>%
    httr2::req_headers(accept = "application/json") %>%
    httr2::req_perform() %>%
    httr2::resp_body_json() %>%
    dplyr::bind_rows()

  if(with_geometries){
    out <- out %>% gaz_add_geometry()
  }

  out

}


#' Get all gazetteer records where the geometry intersects with the given latitude and longitude
#'
#' @param latitude A decimal number which ranges from -90 to 90. Coordinates are assumed to be in WGS84
#' @param longitude A decimal number which ranges from -180 to 180. Coordinates are assumed to be in WGS84
#' @param with_geometries Logical. Add geometries to the result data frame? Default = FALSE
#' @param typeid Restrict to one or more placetypeIDs. See function gaz_rest_types() to retrieve a list of placetypeIDs. Default = NULL
#'
#' @export
#'
#' @examples
#' gaz_rest_records_by_lat_long(51.21551, 2.927)
#' gaz_rest_records_by_lat_long(51.21551, 2.927, with_geometries = TRUE, typeid = c(255, 259))
gaz_rest_records_by_lat_long <- function(latitude, longitude, with_geometries = FALSE, typeid = NULL){

  # Assertions
  checkmate::assert_double(latitude, lower = -90, upper = 90, len = 1)
  checkmate::assert_double(longitude, lower = -180, upper = 180, len = 1)
  typeid = checkmate::assert_integerish(typeid, lower = 1, any.missing = FALSE,
                                        null.ok = TRUE, coerce = TRUE)

  # Config
  url <- glue::glue("https://marineregions.org/rest/getGazetteerRecordsByLatLong.json/{latitude}/{longitude}/")


  # Reusable http request that overrides automatic error check
  get_source <- function(offset){
    req <- httr2::request(url) %>%
      httr2::req_user_agent(mr_user_agent) %>%
      httr2::req_headers(accept = "application/json") %>%
      httr2::req_url_query(typeID = paste0(as.character(typeid), collapse = ","),
                           offset = offset) %>%
      httr2::req_error(is_error = function(resp) FALSE) %>%
      httr2::req_perform()
  }

  # First request - will work as placeholder
  offset = 0
  resp <- get_source(offset)

  # Check status: first offset should be 200
  if(httr2::resp_is_error(resp)){

    # If first is 404, either the typeID is not correct or the name does not exists
    if(httr2::resp_status(resp) == 404){

      # base msg
      msg <- c(
        "x" = glue::glue("There are no matches for point with latitude: '{latitude}' and longitude: '{longitude}'")
      )

      if(!is.null(typeid)){
        is_not_typeid <- !(all(typeid %in% gaz_rest_types()$typeID))
        if(is_not_typeid){
          stop(glue::glue("`typeid` must be element of set `gaz_rest_types()`, but is '{typeid}'"), call. = FALSE)
        }

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

  }else{

    # If all ok, continue with first offset
    resp <- get_source(offset) %>%
      httr2::resp_body_json() %>%
      dplyr::bind_rows()

    # If less than 100 rows, means no need to offset, avoid next request as they are expensive
    if(nrow(resp) < 100){
      resp <- resp %>% dplyr::arrange(MRGID)

      if(with_geometries){
        resp <- resp %>% gaz_add_geometry()
      }

      return(resp)
    }

    # If 100 rows or more, enter infinite loop
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

        if(with_geometries){
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

