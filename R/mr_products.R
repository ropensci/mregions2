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
  ) %>% dplyr::transmute(
    title, layer = product_layer, license, citation, doi, imis, abstract
  )
}

#' Available data products at MarineRegions
#'
#' @return Tibble of available data products via MarineRegions
#'
#' @examples
#' mr_products()
#' @export
mr_products <- memoise::memoise(.mr_products)
