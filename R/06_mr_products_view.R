mr_product_view <- function(product_name){

  # Assertions
  checkmate::assert_character(product_name, len = 1)
  checkmate::assert_choice(product_name, mr_products()$layer)

  # Config
  layer <- subset(mr_products()$id, mr_products()$layer == product_name)
  layer <- strsplit(layer[[1]], ":", TRUE)[[1]]
  namespace <- layer[1]
  layer <- layer[2]
  wms <- glue::glue("http://geo.vliz.be/geoserver/{namespace}/wms?")

  # Server check
  httr2::request(wms) %>%
    httr2::req_url_path_append("request=GetCapabilities") %>%
    httr2::req_method("HEAD") %>%
    httr2::req_user_agent(mr_user_agent) %>%
    httr2::req_perform() %>%
    httr2::resp_check_status()

  # Perform
  mr_map <- base_map() %>%
    leaflet.extras2::addWMS(
      baseUrl = wms,
      layers = layer,
      options = leaflet::WMSTileOptions(
        transparent = TRUE,
        format = "image/png",
        info_format = "text/html"
      ),
      attribution = shiny::HTML("<a href='https://marineregions.org/'>Marine Regions</a>")
    ) %>% add_labels()

  return(mr_map)
}



base_map <- function(){
  emodnet_tiles <-"https://tiles.emodnet-bathymetry.eu/2020/baselayer/inspire_quad/{z}/{x}/{y}.png"

  # Assertions
  z=1;x=1;y=1
  url_test <- glue::glue(emodnet_tiles)
  url_not_up <- httr2::request(url_test) %>%
    httr2::req_method("HEAD") %>%
    httr2::req_user_agent(mr_user_agent) %>%
    httr2::req_perform() %>%
    httr2::resp_is_error()

  if(url_not_up){
    cli::cli_abort(c(
      "x" = "Connection to {.val {url_test}} failed",
      "i" = "Check status of {.val https://portal.emodnet-bathymetry.eu/}"
    ))
  }

  # Perform
  base_map <- leaflet::leaflet(
    options = leaflet::leafletOptions(crs = leaflet::leafletCRS("L.CRS.EPSG4326"))
  ) %>%
    leaflet::addTiles(
      urlTemplate = emodnet_tiles,
      options = leaflet::tileOptions(tms = FALSE),
      attribution = shiny::HTML("<a href='https://emodnet.ec.europa.eu'>EMODnet</a>")
    )

  return(base_map)
}

add_labels <- function(map){
  emodnet_labels <- "https://tiles.emodnet-bathymetry.eu/osm/labels/inspire_quad/{z}/{x}/{y}.png"
  map %>%
    leaflet::addTiles(urlTemplate = emodnet_labels,
                      options = leaflet::tileOptions(tms = FALSE)
    )
}
