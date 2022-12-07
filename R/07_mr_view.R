#' Visualize a Marine Regions data product without downloading
#'
#' @param data_product identifier of the data product. See [mr_list()]
#' @param cql_filter Contextual Query Language (CQL) filter. See details.
#' @param filter standard OGC filter specification. See details.
#' @param ... pass the `cql_filter` and `filter` parameters to `mr_view()` when using one of the helpers
#'
#' @details
#'   This function uses \href{https://en.wikipedia.org/wiki/Web_Map_Service}{WMS services} to load quickly
#'   a Leaflet viewer of a Marine Regions data product. It uses \href{https://emodnet.ec.europa.eu}{EMODnet Bathymetry} as background layer.
#'
#'
#'   # Filters
#'
#'   Both the \href{https://portal.ogc.org/files/96288}{Contextual Query Language (CQL) filter} and the \href{https://www.ogc.org/standards/filter}{standard OGC filter specification} allow to
#'   query the server before performing a request. This will boost performance as you will only retrieve the area of your interest. It is possible to query on attributes, but also perform
#'   geospatial queries. For instance, you can query a bounding box of interest.
#'
#'   CQL filters are possible only in geoserver. Marine Regions uses a geoserver instance to serve its data products.
#'   A tutorial on CQL filters is available in the \href{https://docs.geoserver.org/stable/en/user/tutorials/cql/cql_tutorial.html}{geoserver web site}.
#'
#'
#'   # Helpers
#'
#'   A series of helpers was made available to ease the visualization of the data products, try \code{mr_view_*()}
#'   with the identifier of the data product (see [mr_list()])
#'
#' @export
#'
#' @seealso mr_list(), mr_colnames(), mr_get()
#'
#' @examples
#' \dontrun{
#' # You can pass a product name from `mr_list()`
#' mr_view('eez')
#'
#' Or use the helper
#' mr_view_eez()
#'
#' # Example: filter a the Ecoregions 'Azores Canaries Madeira' with mrgid 21885
#' # You can check the names of the columns beforehand with mr_colnames('ecoregions')
#' mr_view_ecoregions(filter = "<Filter><PropertyIsEqualTo><PropertyName>ecoregion</PropertyName><Literal>Azores Canaries Madeira</Literal></PropertyIsEqualTo></Filter>")
#'
#' # OGC filter are very verbose... but luckily you can use a CQL filter instead
#' mr_view_ecoregions(cql_filter = "ecoregion = 'Azores Canaries Madeira'")
#'
#' # View all the Extended Continental Shelf (ECS) boundary lines published during the first
#' # decade of the 21st century
#' mr_view_ecs_boundaries(cql_filter="doc_date > '2000-01-01' AND doc_date < '2009-12-31'")
#'
#' # Or as timestamp
#' mr_view_eez_boundaries(cql_filter="doc_date AFTER 2000-01-01T00:00:00Z AND doc_date BEFORE 2009-12-31T00:00:00Z")
#' }
mr_view <- function(data_product, cql_filter = NULL, filter = NULL){

  # Assertions
  checkmate::assert_character(data_product, len = 1)
  checkmate::assert_choice(data_product, mr_list()$data_product)

  both_filters_given <- hasArg(cql_filter) & hasArg(filter)
  if(both_filters_given) stop("You must provide one of `cql_filter` or `filter`, not both.", call. = FALSE)

  checkmate::assert_character(cql_filter, null.ok = TRUE, len = 1)
  checkmate::assert_character(filter, null.ok = TRUE, len = 1)


  # Config
  layer <- subset(mr_list()$id, mr_list()$data_product == data_product)
  layer <- strsplit(layer[[1]], ":", TRUE)[[1]]
  namespace <- layer[1]
  layer <- layer[2]
  wms <- glue::glue("https://geo.vliz.be/geoserver/{namespace}/wms?")

  # Server check
  httr2::request(wms) %>%
    httr2::req_url_path_append("request=GetCapabilities") %>%
    httr2::req_method("HEAD") %>%
    httr2::req_user_agent(mr_user_agent) %>%
    httr2::req_perform() %>%
    httr2::resp_check_status()


  # Add filters
  if(hasArg(cql_filter)){
    wms <- paste0(wms, "cql_filter=", URLencode(cql_filter))
  }

  if(hasArg(filter)){
    wms <- paste0(wms, "filter=", URLencode(filter))
  }

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
mr_view_eez <- function(...) mr_view('eez', ...)

#' @rdname mr_view
#' @export
mr_view_eez_boundaries <- function(...) mr_view('eez_boundaries', ...)

#' @rdname mr_view
#' @export
mr_view_eez_12nm <- function(...) mr_view('eez_12nm', ...)

#' @rdname mr_view
#' @export
mr_view_eez_24nm <- function(...) mr_view('eez_24nm', ...)

#' @rdname mr_view
#' @export
mr_view_eez_internal_waters <- function(...) mr_view('eez_internal_waters', ...)

#' @rdname mr_view
#' @export
mr_view_eez_archipelagic_waters <- function(...) mr_view('eez_archipelagic_waters', ...)

#' @rdname mr_view
#' @export
mr_view_high_seas <- function(...) mr_view('high_seas', ...)

#' @rdname mr_view
#' @export
mr_view_ecs <- function(...) mr_view('ecs', ...)

#' @rdname mr_view
#' @export
mr_view_ecs_boundaries <- function(...) mr_view('ecs_boundaries', ...)

#' @rdname mr_view
#' @export
mr_view_iho <- function(...) mr_view('iho', ...)

#' @rdname mr_view
#' @export
mr_view_goas <- function(...) mr_view('goas', ...)

#' @rdname mr_view
#' @export
mr_view_eez_iho <- function(...) mr_view('eez_iho', ...)

#' @rdname mr_view
#' @export
mr_view_eez_land <- function(...) mr_view('eez_land', ...)

#' @rdname mr_view
#' @export
mr_view_longhurst <- function(...) mr_view('longhurst', ...)

#' @rdname mr_view
#' @export
mr_view_cds <- function(...) mr_view('cds', ...)

#' @rdname mr_view
#' @export
mr_view_eca_reg13_nox <- function(...) mr_view('eca_reg13_nox', ...)

#' @rdname mr_view
#' @export
mr_view_eca_reg14_sox_pm <- function(...) mr_view('eca_reg14_sox_pm', ...)

#' @rdname mr_view
#' @export
mr_view_worldheritagemarineprogramme <- function(...) mr_view('worldheritagemarineprogramme', ...)

#' @rdname mr_view
#' @export
mr_view_lme <- function(...) mr_view('lme', ...)


#' @rdname mr_view
#' @export
mr_view_ecoregions <- function(...) mr_view('ecoregions', ...)

#' @rdname mr_view
#' @export
mr_view_seavox_v18 <- function(...) mr_view('seavox_v18', ...)

