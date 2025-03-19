#' Visualize a Marine Regions data product without downloading.
#'
#' @description
#'   Visualize a Marine Regions data product without downloading.
#'
#'   A series of helpers are available to ease the selection of the data products. Example: instead of running
#'
#'   `mrp_view("eez")`
#'
#'   You can use
#'
#'   `mrp_view_eez()`
#'
#'   Try `mrp_view_*()`with the identifier of the data product (see [mrp_list])
#'
#' @param layer (character) Identifier of the data product. See [mrp_list]
#' @param cql_filter (character) Contextual Query Language (CQL) filter. See details.
#' @param filter (character) Standard OGC filter specification. See details.
#' @param ... pass the `cql_filter` and `filter` parameters to [mrp_view()] when using one of the helpers
#'
#' @details
#'   This function uses [WMS services](https://en.wikipedia.org/wiki/Web_Map_Service) to load quickly
#'   a Leaflet viewer of a Marine Regions data product. It uses the [EMODnet Bathymetry](https://emodnet.ec.europa.eu/en) Digital Terrain Model as background layer.
#'
#'   ## Filters
#'
#'   Both the [Contextual Query Language (CQL) filter](https://portal.ogc.org/files/96288) and the [standard OGC filter specification](https://www.ogc.org/publications/standard/filter/) allow to
#'   query the server before performing a request. This will boost performance as you will only retrieve the area of your interest. It is possible to query on attributes, but also perform
#'   geospatial queries. For instance, you can query a bounding box of interest.
#'
#'   CQL filters are possible only in geoserver. Marine Regions uses a geoserver instance to serve its data products.
#'   A tutorial on CQL filters is available in the [geoserver web site](https://docs.geoserver.org/stable/en/user/tutorials/cql/cql_tutorial.html).
#'
#' @export
#'
#' @seealso [mrp_list] to describe the list of products, [mrp_colnames()] and [mrp_col_unique()] to get the name, data type and unique values of a the columns of a data product, useful to query
#' with the arguments `cql_filter` or `filter`, [mrp_get()] to get the data products as a [simple feature](https://r-spatial.github.io/sf/) object.
#'
#' @return A leaflet map with a data product visualized via WMS
#'
#' @examples \donttest{
#' # You can pass a product name from mrp_list
#' mrp_view('eez')
#'
#' # Or use the helper
#' mrp_view_eez()
#'
#' # Example: filter a the Ecoregions 'Azores Canaries Madeira' with mrgid 21885
#' # You can check the names of the columns beforehand with mrp_colnames('ecoregions')
#' mrp_view_ecoregions(filter = "
#'   <Filter>
#'     <PropertyIsEqualTo>
#'       <PropertyName>ecoregion</PropertyName>
#'       <Literal>Azores Canaries Madeira</Literal>
#'     </PropertyIsEqualTo>
#'   </Filter>
#' ")
#'
#' # OGC filter are very verbose... but luckily you can use a CQL filter instead
#' mrp_view_ecoregions(cql_filter = "ecoregion = 'Azores Canaries Madeira'")
#'
#' # View all the Extended Continental Shelf (ECS) boundary lines published during the first
#' # decade of the 21st century
#' mrp_view_ecs_boundaries(
#'   cql_filter = "doc_date > '2000-01-01' AND doc_date < '2009-12-31'"
#' )
#'
#' # Or as timestamp
#' mrp_view_eez_boundaries(
#'   cql_filter = "doc_date AFTER 2000-01-01T00:00:00Z AND doc_date BEFORE 2009-12-31T00:00:00Z"
#' )
#'}
mrp_view <- function(layer, cql_filter = NULL, filter = NULL){

  # Assertions
  assert_deps(c("leaflet", "leaflet.extras2"))
  checkmate::assert_character(layer, len = 1)
  checkmate::assert_choice(layer, mregions2::mrp_list$layer)
  assert_only_one_filter(cql_filter, filter)
  checkmate::assert_character(cql_filter, null.ok = TRUE, len = 1)
  checkmate::assert_character(filter, null.ok = TRUE, len = 1)
  assert_internet()


  # Config
  namespace <- subset(mregions2::mrp_list$namespace, mregions2::mrp_list$layer == layer)
  wms <- glue::glue("https://geo.vliz.be/geoserver/{namespace}/wms?")

  # Server check
  assert_service(wms)

  # Add filters
  if(methods::hasArg(cql_filter)){
    wms <- paste0(wms, "cql_filter=", utils::URLencode(cql_filter))
  }

  if(methods::hasArg(filter)){
    wms <- paste0(wms, "filter=", utils::URLencode(filter))
  }


  # Add HTML class to citation
  cite_mr <- "<a href='https://marineregions.org/'>Marine Regions</a>"
  attr(cite_mr, "class") <- c("html", "character")

  # Perform
  mrp_map <- base_map() %>%
    leaflet.extras2::addWMS(
      baseUrl = wms,
      layers = layer,
      options = leaflet::WMSTileOptions(
        transparent = TRUE,
        format = "image/png",
        info_format = "text/html"
      ),
      attribution = cite_mr
    ) %>% add_labels()

  mrp_list <- NULL

  return(mrp_map)
}

assert_deps <- function(deps){
  check_if_missing <- function(pkg){
    !requireNamespace(pkg, quietly = TRUE)
  }

  deps <- data.frame(pkgname = deps)
  deps$missing <- lapply(deps$pkgname, check_if_missing) %>% unlist()

  if(any(deps$missing)){
    missing_pkgs <- subset(deps$pkgname, deps$missing == TRUE)
    cli::cli_abort(c(
      "!" = "{.fun mrp_view} requires the {.pkg {missing_pkgs}} package{?s} but {?is/are} not installed"
    ))
  }
}

base_map <- function(){
  emodnet_tiles <-"https://tiles.emodnet-bathymetry.eu/2020/baselayer/inspire_quad/{z}/{x}/{y}.png"

  # Assert
  z<-1;x<-1;y<-1
  url_test <- glue::glue(emodnet_tiles)
  assert_service(url_test)

  # Add HTML class to citation
  cite_emodnet <- "<a href='https://emodnet.ec.europa.eu/en'>EMODnet</a>"
  attr(cite_emodnet, "class") <- c("html", "character")

  # Perform
  base_map <- leaflet::leaflet(
    options = leaflet::leafletOptions(crs = leaflet::leafletCRS("L.CRS.EPSG4326"))
  ) %>%
    leaflet::addTiles(
      urlTemplate = emodnet_tiles,
      options = leaflet::tileOptions(tms = FALSE),
      attribution = cite_emodnet
    )

  return(base_map)
}

add_labels <- function(map){
  emodnet_labels <- "https://tiles.emodnet-bathymetry.eu/osm/labels/inspire_quad/{z}/{x}/{y}.png"

  # Assert
  z<-1;x<-1;y<-1
  url_test <- glue::glue(emodnet_labels)
  assert_service(url_test)

  # Perform
  map %>%
    leaflet::addTiles(urlTemplate = emodnet_labels,
                      options = leaflet::tileOptions(tms = FALSE)
    )
}


#' @rdname mrp_view
#' @export
mrp_view_eez <- function(...) mrp_view('eez', ...)

#' @rdname mrp_view
#' @export
mrp_view_eez_boundaries <- function(...) mrp_view('eez_boundaries', ...)

#' @rdname mrp_view
#' @export
mrp_view_eez_12nm <- function(...) mrp_view('eez_12nm', ...)

#' @rdname mrp_view
#' @export
mrp_view_eez_24nm <- function(...) mrp_view('eez_24nm', ...)

#' @rdname mrp_view
#' @export
mrp_view_eez_internal_waters <- function(...) mrp_view('eez_internal_waters', ...)

#' @rdname mrp_view
#' @export
mrp_view_eez_archipelagic_waters <- function(...) mrp_view('eez_archipelagic_waters', ...)

#' @rdname mrp_view
#' @export
mrp_view_high_seas <- function(...) mrp_view('high_seas', ...)

#' @rdname mrp_view
#' @export
mrp_view_ecs <- function(...) mrp_view('ecs', ...)

#' @rdname mrp_view
#' @export
mrp_view_ecs_boundaries <- function(...) mrp_view('ecs_boundaries', ...)

#' @rdname mrp_view
#' @export
mrp_view_iho <- function(...) mrp_view('iho', ...)

#' @rdname mrp_view
#' @export
mrp_view_goas <- function(...) mrp_view('goas', ...)

#' @rdname mrp_view
#' @export
mrp_view_eez_iho <- function(...) mrp_view('eez_iho', ...)

#' @rdname mrp_view
#' @export
mrp_view_eez_land <- function(...) mrp_view('eez_land', ...)

#' @rdname mrp_view
#' @export
mrp_view_longhurst <- function(...) mrp_view('longhurst', ...)

#' @rdname mrp_view
#' @export
mrp_view_cds <- function(...) mrp_view('cds', ...)

#' @rdname mrp_view
#' @export
mrp_view_eca_reg13_nox <- function(...) mrp_view('eca_reg13_nox', ...)

#' @rdname mrp_view
#' @export
mrp_view_eca_reg14_sox_pm <- function(...) mrp_view('eca_reg14_sox_pm', ...)

#' @rdname mrp_view
#' @export
mrp_view_worldheritagemarineprogramme <- function(...) mrp_view('worldheritagemarineprogramme', ...)

#' @rdname mrp_view
#' @export
mrp_view_lme <- function(...) mrp_view('lme', ...)

#' @rdname mrp_view
#' @export
mrp_view_ecoregions <- function(...) mrp_view('ecoregions', ...)

#' @rdname mrp_view
#' @export
mrp_view_seavox_v18 <- function(...) mrp_view('seavox_v18', ...)

