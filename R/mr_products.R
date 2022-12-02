.mr_products <- function() {

  mr_products <- readr::read_delim(
    system.file("mr_products.csv", package = "mregions2"),
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
  caps_ft <- purrr::map(mr_products$geoserverID, ~ capabilities$findFeatureTypeByName(.x))

  mr_products %>% dplyr::mutate(
    title = purrr::map_chr(caps_ft, ~ .x$getTitle()),
    abstract = purrr::map_chr(caps_ft, ~ .x$getAbstract())
  ) %>% dplyr::select(
    title,
    layer = product_layer,
    license,
    citation,
    doi,
    imis,
    abstract,
    id = geoserverID
  )
}

#' Available layers at MarineRegions
#'
#' @export
#'
#' @description
#' A function returning  data frame including the name, abstract and
#'   some other relevant about each layer.
#'
#' * `title`: name of the layer
#' * `layer`: identifier of the layer. Use in [mr_product_get()]
#' * `license`: terms of use of the layer
#' * `citation`: preferred citation of the layer
#' * `doi`: ISO 26324 \href{https://doi.org}{Digital Object Identifier}
#' * `imis`: url of the layer in the \href{https://vliz.be/imis?}{Integrated Marine Information System (IMIS)}
#' * `id`: geoserver ID, reserve for internal use. Format 'namespace:layer'
#'
mr_products <- memoise::memoise(.mr_products)
