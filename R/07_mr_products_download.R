#' Get product
#'
#' @param product_name
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' mr_product_get("MarineRegions:eez_boundaries")
mr_product_get <- function(product_name, ...){

  checkmate::assert_character(product_name, len = 1)

  # Get WFS connection and capabilities
  wfs <- mr_init_wfs_client(silent = TRUE)

  out <- wfs$getFeatures(product_name, ...)

  # Fix Geometry type
  out

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
mr_init_wfs_client <- function(version = "2.0.0", silent = FALSE){
  checkmate::assert_character(version)
  checkmate::assert_choice(version, c("1.0.0", "1.1.1", "2.0.0"))

  # Perform
  url <- "https://geo.vliz.be/geoserver/wfs"

  try({
    wfs <- ows4R::WFSClient$new(url, version, logger = NULL)
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
