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


#' List of all products available via MarineRegions
#'
#' @return a tibble
#' @export
#'
#' @examples
#' mr_products_info()
mr_product_info <- function(){

  # Get WFS connection and capabilities
  wfs <- mr_init_wfs_client(silent = TRUE)
  capabilities <- wfs$getCapabilities()
  caps_ft <- purrr::map(products$layer_name, ~ capabilities$findFeatureTypeByName(.x))

  # Create list dataset
  products <- products %>% dplyr::mutate(
    title = purrr::map_chr(caps_ft, ~ .x$getTitle()),
    abstract = purrr::map_chr(caps_ft, ~ .x$getAbstract())
  ) %>%
    tidyr::separate(.data$layer_name,
                    into = c("namespace", "layer"),
                    sep = ":"
  )

  products

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




# hard coded list of data products
products <- tibble::tibble(
  product_family = c(
    "Maritime Boundaries",
    "Maritime Boundaries",
    "Maritime Boundaries",
    "Maritime Boundaries",
    "Maritime Boundaries",
    "Maritime Boundaries",
    "Maritime Boundaries",
    "Maritime Boundaries",
    "Maritime Boundaries",
    "IHO Sea Areas",
    "Global Oceans and Seas",
    "Marineregions: intersect of EEZs and IHO areas",
    "Marine and land zones: the union of world country boundaries and EEZ's",
    "Longhurst Provinces",
    "Global contourite distribution Shapefile",
    "Emission Control Areas (ECAs)",
    "Emission Control Areas (ECAs)",
    "World Marine Heritage Sites",
    "Large Marine Ecosystems of the World",
    "Marine Ecoregions of the World, MEOW (Spalding et al., 2007)",
    "The SeaVoX Salt and Fresh Water Body Gazetteer",
    "The SeaVoX Salt and Fresh Water Body Gazetteer",
    "The SeaVoX Salt and Fresh Water Body Gazetteer"
  ),

  layer_name = c(
    "MarineRegions:eez",
    "MarineRegions:eez_boundaries",
    "MarineRegions:eez_12nm",
    "MarineRegions:eez_24nm",
    "MarineRegions:eez_internal_waters",
    "MarineRegions:eez_archipelagic_waters",
    "MarineRegions:high_seas",
    "MarineRegions:ecs",
    "MarineRegions:ecs_boundaries",
    "MarineRegions:iho",
    "MarineRegions:goas",
    "MarineRegions:eez_iho",
    "MarineRegions:eez_land",
    "MarineRegions:longhurst",
    "World:cds",
    "MarineRegions:eca_reg13_nox",
    "MarineRegions:eca_reg14_sox_pm",
    "MarineRegions:worldheritagemarineprogramme",
    "MarineRegions:lme",
    "Ecoregions:ecoregions",
    "MarineRegions:seavox_v16",
    "MarineRegions:seavox_v17",
    "MarineRegions:seavox_v18"
  )
)
