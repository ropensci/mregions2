.mrp_list <- function() {
  # Avoid "no visible binding for global variable" R CMD Check NOTE
  title <- abstract <- data_product <- product_layer <- geoserverID <- NULL
  license <- citation <- doi <- imis <- abstract <- id <- NULL

  mrp_list <- utils::read.delim2(
    system.file("mrp_list.csv", package = "mregions2"),
    header = TRUE,
    sep = ";",
    dec = ".",
    fileEncoding = "UTF-8"
  ) %>% dplyr::mutate(
    geoserverID = glue::glue("{product_namespace}:{product_layer}")
  )

  # Get Info from WFS
  wfs <- mrp_init_wfs_client(silent = TRUE)
  capabilities <- wfs$getCapabilities()
  caps_ft <- purrr::map(mrp_list$geoserverID, ~ capabilities$findFeatureTypeByName(.x))

  mrp_list <- mrp_list %>% dplyr::mutate(
    title = purrr::map_chr(caps_ft, ~ .x$getTitle()),
    abstract = purrr::map_chr(caps_ft, ~ .x$getAbstract())
  ) %>% dplyr::select(
    title,
    data_product = product_layer,
    license,
    citation,
    doi,
    imis,
    abstract,
    id = geoserverID
  )

  # Turn into tibble
  attr(mrp_list, "class") <- c("tbl_df", "tbl", "data.frame")

  mrp_list
}

#' Available data products at Marine Regions
#'
#' @export
#'
#' @description
#' A function returning data frame including the name, abstract and
#'   some other relevant about each data product in Marine Regions.
#'
#' * `title`: name of the data products
#' * `data_product`: identifier of the data product. Use in [mrp_get()]
#' * `license`: terms of use of the data products
#' * `citation`: preferred citation of the data products
#' * `doi`: ISO 26324 [Digital Object Identifier](https://doi.org)
#' * `imis`: url of the data products in the [Integrated Marine Information System (IMIS)](https://vliz.be/imis?)
#' * `id`: geoserver ID, reserved for internal use. Format 'namespace:data_product'
#'
#' @seealso [mrp_view()], [mrp_colnames()], [mrp_get()]
#'
#' @references
#' * Sources: <https://marineregions.org/sources.php>
#' * Methodology of the creation of the Maritime Boundaries: <https://marineregions.org/eezmethodology.php>
#' * Direct download: <https://marineregions.org/downloads.php>
mrp_list <- memoise::memoise(.mrp_list)


.mrp_init_wfs_client <- function(version = "2.0.0", silent = FALSE, ...){
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
#' @param version (character) The WFS version. Supported: `c("1.0.0", "1.1.1", "2.0.0")`
#' @param silent (logical) Hide success or fail message? Default = FALSE
#' @param ... params to be passed to `ows4R::WFSClient$new`
#'
#' @return An object of class `c("WFSClient", "OWSClient", "OGCAbstractObject","R6")`. See package [ows4R].
#' @export
#'
#' @examples
#' wfs <- mrp_init_wfs_client()
mrp_init_wfs_client <- memoise::memoise(.mrp_init_wfs_client)
