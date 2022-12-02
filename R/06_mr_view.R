#' Visualize a Marine Regions layer fast
#'
#' @param product_name the layer, see [mr_products()]
#'
#' @details
#'   This function uses WMS services to load quickly a Leaflet viewer of a Marine Regions layer.
#'   It uses EMODnet Bathymetry as background layer.
#'
#'   A series of helpers was made available to ease the visualization of the layers, try \code{mr_view_*()}
#'   with the name of the layer.
#'
#' @export
#'
#' @examples
#' mr_view('eez')
#' mr_view_eez()
mr_view <- function(product_name){

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


#' @rdname mr_view
#' @export
mr_view_eez <- function() mr_view('eez')

#' @rdname mr_view
#' @export
mr_view_eez_boundaries <- function() mr_view('eez_boundaries')

#' @rdname mr_view
#' @export
mr_view_eez_12nm <- function() mr_view('eez_12nm')

#' @rdname mr_view
#' @export
mr_view_eez_24nm <- function() mr_view('eez_24nm')

#' @rdname mr_view
#' @export
mr_view_eez_internal_waters <- function() mr_view('eez_internal_waters')

#' @rdname mr_view
#' @export
mr_view_eez_archipelagic_waters <- function() mr_view('eez_archipelagic_waters')

#' @rdname mr_view
#' @export
mr_view_high_seas <- function() mr_view('high_seas')

#' @rdname mr_view
#' @export
mr_view_ecs <- function() mr_view('ecs')

#' @rdname mr_view
#' @export
mr_view_ecs_boundaries <- function() mr_view('ecs_boundaries')

#' @rdname mr_view
#' @export
mr_view_iho <- function() mr_view('iho')

#' @rdname mr_view
#' @export
mr_view_goas <- function() mr_view('goas')

#' @rdname mr_view
#' @export
mr_view_eez_iho <- function() mr_view('eez_iho')

#' @rdname mr_view
#' @export
mr_view_eez_land <- function() mr_view('eez_land')

#' @rdname mr_view
#' @export
mr_view_longhurst <- function() mr_view('longhurst')

#' @rdname mr_view
#' @export
mr_view_cds <- function() mr_view('cds')

#' @rdname mr_view
#' @export
mr_view_eca_reg13_nox <- function() mr_view('eca_reg13_nox')

#' @rdname mr_view
#' @export
mr_view_eca_reg14_sox_pm <- function() mr_view('eca_reg14_sox_pm')

#' @rdname mr_view
#' @export
mr_view_worldheritagemarineprogramme <- function() mr_view('worldheritagemarineprogramme')

#' @rdname mr_view
#' @export
mr_view_lme <- function() mr_view('lme')

#' @rdname mr_view
#' @export
mr_view_ecoregions <- function() mr_view('ecoregions')

#' @rdname mr_view
#' @export
mr_view_seavox_v18 <- function() mr_view('seavox_v18')

