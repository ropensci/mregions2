.mr_list <- function() {

  mr_list <- readr::read_delim(
    system.file("mr_list.csv", package = "mregions2"),
    col_types = readr::cols(
      readr::col_character()
    ),
    delim = ";"
  ) %>% dplyr::mutate(
    geoserverID = glue::glue("{product_namespace}:{product_layer}")
  )

  # Get Info from WFS
  wfs <- mr_init_wfs_client(silent = TRUE)
  capabilities <- wfs$getCapabilities()
  caps_ft <- purrr::map(mr_list$geoserverID, ~ capabilities$findFeatureTypeByName(.x))

  mr_list %>% dplyr::mutate(
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
#' * `data_product`: identifier of the data product. Use in [mr_get()]
#' * `license`: terms of use of the data products
#' * `citation`: preferred citation of the data products
#' * `doi`: ISO 26324 \href{https://doi.org}{Digital Object Identifier}
#' * `imis`: url of the data products in the \href{https://vliz.be/imis?}{Integrated Marine Information System (IMIS)}
#' * `id`: geoserver ID, reserved for internal use. Format 'namespace:data_product'
#'
#' @seealso mr_view(), mr_colnames(), mr_get()
#'
#' @references
#' Sources: \url{https://marineregions.org/sources.php}
#' Methodology of the creation of the Maritime Boundaries: \url{https://marineregions.org/eezmethodology.php}
#' Direct download: \url{https://marineregions.org/downloads.php}
mr_list <- memoise::memoise(.mr_list)
