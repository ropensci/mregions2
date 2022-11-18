library(magrittr)

mr_gaz_wmses <- function(mrgid){
  # Assertions
  checkmate::assert_int(mrgid, lower = 1)

  # Config
  url <- glue::glue("https://marineregions.org/rest/getGazetteerWMSes.json/{mrgid}/")

  # Perform
  httr2::request(url) %>%
    httr2::req_user_agent("mregions2") %>%
    httr2::req_perform() %>%
    httr2::resp_body_json() %>%
    dplyr::bind_rows()

}



mr_view <- function(mrgid){

  # Config
  base_map <- leaflet::leaflet() %>%
    leaflet::addTiles()

  # Build OGC filter
  wms_list <- mr_gaz_wmses(mrgid) %>%
    dplyr::mutate(
      ogc = glue::glue("<PropertyIsEqualTo><PropertyName>{featureName}</PropertyName><Literal>{value}</Literal></PropertyIsEqualTo>")
    ) %>%
    dplyr::group_by(url, namespace, featureType, featureName) %>%
    dplyr::mutate(
      add_or = ifelse(dplyr::row_number() < 3, "", "</Or>"),
      ogc = paste0(add_or, ogc)
    ) %>%
    dplyr::summarise(
      ogc = paste0(ogc, collapse = ""),
      n = dplyr::n() - 1
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      ogc = paste0(paste0(rep('<Or>', times = n), collapse = ""), ogc)
    ) %>%
    dplyr::mutate(
      ogc = ifelse(n > 1, paste0(ogc, "</Or>"), ogc),
      ogc = paste0(url, "filter=<Filter>", ogc, "</Filter>")
    )

  # Perform
  map <- list()

  for(i in nrow(wms_list)){
    map[[i]] <- base_map %>%
      leaflet.extras2::addWMS(
        baseUrl = wms_list$ogc,
        layers = wms_list$featureType,
        options = leaflet::WMSTileOptions(
          transparent = TRUE,
          format = "image/png",
          info_format = "text/html"
        )
      )
  }

  map
}

# this works
mr_view(8761)

# this fails
# t <- mr_view(58)






