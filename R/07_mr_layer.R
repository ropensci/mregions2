#' Get data product
#'
#' @param product_name
#' @param ... any params to be passed to $getFeatures()
#'
#' @return
#' @export
#'
#' @examples
#' mr_product_get("eez_boundaries", cql_filter = "territory1 = 'Spain'")
mr_product_get <- function(product_name, ...){

  checkmate::assert_character(product_name, len = 1)
  checkmate::assert_choice(product_name, mr_products()$layer)

  # Config
  info <- mr_products() %>%
    dplyr::filter(layer == product_name)

  # Perform request
  out <- mr_init_wfs_client(silent = TRUE)$
    getFeatures(info$id, ...)

  # Fix Geometry type
  # Geoserver often returns exotic geometries
  # It is better to turn into MULTILINESTRING / MULTIPOLYGON
  geometry_class <- class(sf::st_geometry(out))

  if("sfc_MULTICURVE" %in% geometry_class){
    try({
      out <- out %>% sf::st_cast("MULTILINESTRING")
    })
  }

  if("sfc_MULTISURFACE" %in% geometry_class){
    try({
      out <- out %>%
        sf::st_cast("GEOMETRYCOLLECTION") %>%
        dplyr::mutate(id = seq_len(nrow(.))) %>%
        sf::st_collection_extract("POLYGON") %>%
        sf:::aggregate.sf(list(.$id), dplyr::first, do_union = FALSE) %>%
        dplyr::select(-id, -Group.1)
    })
  }

  out %>%
    tibble::as_tibble() %>%
    sf::st_as_sf()


}


#' Get the names of the columns and data type of the data product
#'
#' @param layer
#'
#' @details
#'
#' This function becomes useful to filter parts of a product when using mr_product_get().
#'
#' @return
#' @export
#'
#' @examples
mr_product_cols <- function(product_name){

  checkmate::assert_character(product_name, len = 1)
  checkmate::assert_choice(product_name, mr_products()$layer)

  # Config
  info <- mr_products() %>%
    dplyr::filter(layer == product_name)

  # Perform
  mr_init_wfs_client(silent = TRUE)$
    getCapabilities()$
    findFeatureTypeByName(info$id)$
    getDescription(pretty = TRUE) %>%
    dplyr::transmute(column_name = name, type)

}




.mr_init_wfs_client <- function(version = "2.0.0", silent = FALSE, ...){
  checkmate::assert_character(version)
  checkmate::assert_choice(version, c("1.0.0", "1.1.1", "2.0.0"))

  # Perform
  url <- "https://geo.vliz.be/geoserver/wfs"

  try({
    wfs <- ows4R::WFSClient$new(url, version, ...)
  }, silent = TRUE)

  if(exists("wfs")){
    if(!silent){
      cli::cli_alert_success("WFS client created successfully")
      cli::cli_alert_info("Service: {.val {url}}")
      cli::cli_alert_info("Version: {.val {version}}")
    }

    return(wfs)
  }

  # Handle exceptions
  msg <- c("x" = "WFS client creation failed")


  if(!curl::has_internet()){
    cli::cli_abort(c(
      msg,
      "!" = "{.code curl::has_internet()} is {.val FALSE}",
      "i" = "Did you check your internet connection?"
    ))
  }

  resp <- httr2::request(paste0(url, "?request=GetCapabilities")) %>%
    httr2::req_method("HEAD") %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  if(httr2::resp_is_error(resp)){
    cli::cli_abort(c(
      msg,
      "!" = "HTTP {paste0(httr2::resp_status(resp), ' ', httr2::resp_status_desc(resp))}",
      "i" = "Service: {.val {url}}"
    ))
  }


  cli::cli_abort(c(
    msg,
    "!" = "An exception has occurred. Please raise an issue in {.val {packageDescription('mregions2')$BugReports}}"
  ))

}

#' Creates WFS client in Marine Regions
#'
#' @param version The WFS version. Supported: c("1.0.0", "1.1.1", "2.0.0")
#'
#' @return an object of class c("WFSClient", "OWSClient", "OGCAbstractObject","R6"). See package ows4R.
#' @export
#'
#' @examples
#' wfs <- mr_init_wfs_client()
mr_init_wfs_client <- memoise::memoise(.mr_init_wfs_client)
