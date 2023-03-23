# code to prepare mrp_list df

require(magrittr)

  # Read basic info from csv file
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
  url <- "https://geo.vliz.be/geoserver/wfs"
  wfs <- ows4R::WFSClient$new(url, "2.0.0")
  capabilities <- wfs$getCapabilities()
  caps_ft <- purrr::map(mrp_list$geoserverID, ~ capabilities$findFeatureTypeByName(.x))

  mrp_list <- mrp_list %>% dplyr::mutate(
    title = purrr::map_chr(caps_ft, ~ .x$getTitle()),
    abstract = purrr::map_chr(caps_ft, ~ .x$getAbstract())
  ) %>% dplyr::select(
    title,
    namespace = product_namespace,
    layer = product_layer,
    license,
    citation,
    doi,
    imis,
    abstract
  )

  # Turn into tibble
  attr(mrp_list, "class") <- c("tbl_df", "tbl", "data.frame")

usethis::use_data(mrp_list, overwrite = TRUE)
