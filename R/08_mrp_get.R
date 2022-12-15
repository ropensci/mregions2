#' Get a data product
#'
#' @param product_name (character) Identifier of the data product. See [mrp_list()]
#' @param ... Any [vendor params](https://docs.geoserver.org/latest/en/user/services/wfs/vendor.html) to be passed to the [GetFeature](https://docs.geoserver.org/latest/en/user/services/wfs/reference.html?highlight=getfeatures#getfeature) request. See details.
#'
#' @details
#'    This function uses [WFS services](https://en.wikipedia.org/wiki/Web_Feature_Service) to load retrieve the Marine Regions Data products. The data products
#'    are stored in a [geoserver instance](https://geo.vliz.be) hosted and maintained by the [Flanders Marine Institute (VLIZ)](https://www.vliz.be)
#'
#'   # Vendor Params
#'   [Vendor params](https://docs.geoserver.org/latest/en/user/services/wfs/vendor.html) can be passed to a [WFS GetFeature](https://docs.geoserver.org/latest/en/user/services/wfs/reference.html?highlight=getfeatures#getfeature) request.
#'   `mregions2` uses the [ows4R](https://github.com/eblondel/ows4R) package as the [interface to query the WFS service with R.](https://github.com/eblondel/ows4R/wiki#wfs) You can pass any vendor parameters via `...`.
#'   For instance, you can pass CQL or OGC filters with `cql_filter="<the filter>"` or `filter="<the filter>"`. You can change the projection with `srsName="<CRS>"`, only the features in a certain bounding box with `bbox="a1,b1,a2,b2,<CRS>"`
#'   where `a1`, `b1`, `a2` and `b2` are the coordinates and the `CRS` selects the coordinate reference system of this bounding box, or [any other vendor parameters accepted by geoserver](https://docs.geoserver.org/latest/en/user/services/wfs/vendor.html).
#'
#'   # Filtering
#'   Both the [Contextual Query Language (CQL) filter](https://portal.ogc.org/files/96288) and the [standard OGC filter specification](https://www.ogc.org/standards/filter) allow to
#'   query the server before performing a request. This will boost performance as you will only retrieve the area of your interest. It is possible to query on attributes, but also perform
#'   geospatial queries. For instance, you can query a bounding box of interest.
#'
#'   A tutorial on CQL filters is available in the [geoserver web site](https://docs.geoserver.org/stable/en/user/tutorials/cql/cql_tutorial.html).
#'
#' @return An sf object with the Marine Regions data product
#' @export
#'
#' @seealso [mrp_list()] to describe the list of products, [mrp_view()] to visualize the data product in advance, [mrp_colnames()] and [mrp_col_unique()] to get the name, data type and unique values of a the columns of a data product, useful to query
#' with the arguments `cql_filter` or `filter`
#'
#' @examples
#' \dontrun{
#' # See the list of all data products
#' View(mrp_list())
#'
#' # We want the Exclusive Economic Zones of Portugal. Let's first visualize the product:
#' mrp_view("eez")
#'
#' # Let's see all the columns on this data product
#' mrp_colnames("eez")
#'
#' # We should query on sovereign. Let's see all the possible values of sovereign1, sovereign2 and sovereign3
#' sov1 = mrp_col_unique("eez", "sovereign1")
#' sov2 = mrp_col_unique("eez", "sovereign2")
#' sov3 = mrp_col_unique("eez", "sovereign3")
#'
#' # Is Portugal a value in the sovereign1, 2 and 3?
#' "Portugal" %in% sov1
#' #> [1] TRUE
#'
#' "Portugal" %in% sov2
#' #> [1] FALSE
#'
#' "Portugal" %in% sov3
#' #> [1] FALSE
#'
#' # Portugal is only in sovereign1. Let's write a CQL filter to get only the EEZs of Portugal, or those
#' # where Portugal is a party of a dispute or a joint regime
#' portugal_eez <- mrp_get("eez", cql_filter = "sovereign1 = 'Portugal'")
#'
#' # You can also limit the number of features to be requested
#' mrp_get("eez", count = 5)
#' }
mrp_get <- function(product_name, ...){

  checkmate::assert_character(product_name, len = 1)
  checkmate::assert_choice(product_name, mrp_list()$data_product)

  # Config
  info <- mrp_list() %>%
    dplyr::filter(data_product == product_name)

  # Perform request
  ft <- mrp_init_wfs_client(silent = TRUE)$
    getCapabilities()$
    findFeatureTypeByName(info$id)

  out <- ft$getFeatures(...)

  # Fix Geometry type
  # Geoserver often returns exotic geometries
  # It is better to turn into MULTILINESTRING / MULTIPOLYGON
  geometry_class <- class(sf::st_geometry(out))

  if("sfc_MULTICURVE" %in% geometry_class){
    try({
      out <- out %>% sf::st_cast("MULTILINESTRING")
    })
  }

  if("sfc_MULTISURFACE" %in% geometry_class){
    try({
      out <- out %>%
        sf::st_cast("GEOMETRYCOLLECTION") %>%
        dplyr::mutate(id = seq_len(nrow(.))) %>%
        sf::st_collection_extract("POLYGON") %>%
        sf:::aggregate.sf(list(.$id), dplyr::first, do_union = FALSE) %>%
        dplyr::select(-id, -Group.1)
    })
  }


  out <- out %>%
    tibble::as_tibble() %>%
    sf::st_as_sf()

  out$gml_id <- NULL

  if (is.na(sf::st_crs(out))) {
    sf::st_crs(out) <- ft$getDefaultCRS()
  }

  out
}




.mrp_colnames <- function(product_name){

  checkmate::assert_character(product_name, len = 1)
  checkmate::assert_choice(product_name, mrp_list()$data_product)

  # Config
  info <- mrp_list() %>%
    dplyr::filter(data_product == product_name)

  # Perform
  mrp_init_wfs_client(silent = TRUE)$
    getCapabilities()$
    findFeatureTypeByName(info$id)$
    getDescription(pretty = TRUE) %>%
    dplyr::transmute(data_product = product_name, column_name = name, type)

}
#' Get the names of the columns and data type of the data product
#'
#' @param product_name (character) Identifier of the data product. See [mrp_list()]
#'
#' @details
#'   This function becomes useful to write CQL or OGC filters that you can pass to [mrp_get()] or [mrp_view()] as
#'   it allows you to know the column names and the data types beforehand. Use it together with [mrp_col_unique()] to
#'   know all the possible values in the column name that you want to query on.
#'
#'   The actual description of each column is available only to the Maritime Boundaries products.
#'   See <https://marineregions.org/eezattribute.php>
#'
#' @return A data frame with the column names and data type in the Marine Regions data product
#' @export
#'
#' @seealso [mrp_list()] to describe the list of products, [mrp_col_unique()] to get the unique values of a the
#' columns of a data product, useful to write queries that can be passed to [mr_get()] or [mr_view()] via the
#' arguments `cql_filter` or `filter`.
#'
#' @examples
#' mrp_colnames("eez")
#' mrp_colnames("ecoregions")
mrp_colnames <- memoise::memoise(.mrp_colnames)




.mrp_col_unique <- function(product_name, colname){

  checkmate::assert_character(product_name, len = 1)
  checkmate::assert_choice(product_name, mrp_list()$data_product)

  checkmate::assert_character(colname, len = 1)
  colnames <- mrp_colnames(product_name)
  checkmate::assert_choice(colname, colnames[, 2])

  # Geometry column not allowed
  datatype <- tolower(subset(colnames[, 3], colnames[, 2] == colname))
  if(datatype == "geometry"){ stop("`colname` of type geometry are not accepted. See ?mrp_col_unique", call. = FALSE) }

  # Config
  info <- mrp_list() %>%
    dplyr::filter(data_product == product_name)

  url = glue::glue(
    "https://geo.vliz.be/geoserver/wfs?service=wfs&version=2.0.0&request=GetPropertyValue&typeNames={info$id}&valueReference={colname}"
  )

  # Perform
  resp <- httr2::request(url) %>%
    httr2::req_user_agent(mr_user_agent) %>%
    httr2::req_perform() %>%
    httr2::resp_body_xml() %>%
    xml2::xml_find_all(glue::glue("//wfs:member")) %>%
    xml2::xml_text() %>%
    unique()


  if(datatype %in% c("numeric", "integer", "double")) resp <- resp %>% as.numeric()
  if(datatype %in% c("date")) resp <- resp %>% lubridate::as_date()
  if(datatype %in% c("timestamp")) resp <- resp %>% lubridate::as_datetime()

  resp
}
#' Get all the possible values of a column of a Marine Regions data product
#'
#' @param product_name (character) Identifier of the data product. See [mrp_list()]
#' @param colname (character) Column name in the data product. See [mrp_colnames()]
#'
#' @details
#' This function becomes useful to write CQL or OGC filters that you can pass to [mrp_get()] or [mrp_view()] as
#' it helps to know all the possible values in the column name that you want to query on beforehand. Use it
#' together with [mrp_colnames()] to know the columns and data types in the data product.
#'
#' ## Geometry columns
#' Note that columns of type `geometry` are forbidden as their performance is sub-optimal and would likely
#' crash your R session.
#'
#' @return A numeric or character vector with the unique values of a column of a Marine Regions data product.
#' @export
#'
#' @seealso [mrp_list()] to describe the list of products, [mrp_colnames()] to get the names and data type of
#' the columns of a data product, useful to write queries that can be passed to [mr_get()] or [mr_view()] via
#' the arguments `cql_filter` or `filter`.
#'
#' @examples
#' mrp_col_unique("ecs", "pol_type")
#' mrp_col_unique("ecs_boundaries", "line_type")
mrp_col_unique <- memoise::memoise(.mrp_col_unique)

#' @rdname mrp_col_unique
#' @export
mrp_col_distinct <- mrp_col_unique



