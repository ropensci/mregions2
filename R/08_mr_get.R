#' Get data product
#'
#' @param product_name identifier of the data product. See [mr_list()]
#' @param ... any \href{https://docs.geoserver.org/latest/en/user/services/wfs/vendor.html}{vendor params} to be passed to the \href{https://docs.geoserver.org/latest/en/user/services/wfs/reference.html?highlight=getfeatures#getfeature}{GetFeature} request. See details.
#'
#' @details
#'    This function uses \href{https://en.wikipedia.org/wiki/Web_Feature_Service}{WFS services} to load retrieve the Marine Regions Data products. The data products
#'    are stored in a \href{https://geo.vliz.be}{geoserver instance} hosted and maintained by the \href{https://www.vliz.be}{Flanders Marine Institute (VLIZ)}
#'
#'   # Vendor Params
#'   \href{https://docs.geoserver.org/latest/en/user/services/wfs/vendor.html}{Vendor params} can be passed to a \href{https://docs.geoserver.org/latest/en/user/services/wfs/reference.html?highlight=getfeatures#getfeature}{WFS GetFeature} request.
#'   `mregions2` uses the \href{https://github.com/eblondel/ows4R}{ows4R} package as the \href{https://github.com/eblondel/ows4R/wiki#wfs}{interface to query the WFS service with R.} You can pass any vendor parameters via `...`.
#'   For instance, you can pass CQL or OGC filters with `cql_filter="<the filter>"` or `filter="<the filter>"`. You can change the projection with `srsName="<CRS>"`, only the features in a certain bounding box with `bbox="a1,b1,a2,b2,<CRS>"`
#'   where `a1`, `b1`, `a2` and `b2` are the coordinates and the `CRS` selects the coordinate reference system of this bounding box, or \href{https://docs.geoserver.org/latest/en/user/services/wfs/vendor.html}{any other vendor parameters accepted by geoserver}.
#'
#'   # Filtering
#'   Both the \href{https://portal.ogc.org/files/96288}{Contextual Query Language (CQL) filter} and the \href{https://www.ogc.org/standards/filter}{standard OGC filter specification} allow to
#'   query the server before performing a request. This will boost performance as you will only retrieve the area of your interest. It is possible to query on attributes, but also perform
#'   geospatial queries. For instance, you can query a bounding box of interest.
#'
#'   A tutorial on CQL filters is available in the \href{https://docs.geoserver.org/stable/en/user/tutorials/cql/cql_tutorial.html}{geoserver web site}.
#'
#' @return An sf object with the Marine Regions data product
#' @export
#'
#' @examples
#' \dontrun{
#' # See the list of all data products
#' View(mr_list())
#'
#' # We want the Exclusive Economic Zones of Portugal. Let's first visualize the product:
#' mr_view("eez")
#'
#' # Let's see all the columns on this data product
#' mr_colnames("eez")
#'
#' # We should query on sovereign. Let's see all the possible values of sovereign1, sovereign2 and sovereign3
#' sov1 = mr_unique("eez", "sovereign1")
#' sov2 = mr_unique("eez", "sovereign2")
#' sov3 = mr_unique("eez", "sovereign3")
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
#' portugal_eez <- mr_get("eez", cql_filter = "sovereign1 = 'Portugal'")
#' portugal_eez
#' }
mr_get <- function(product_name, ...){

  checkmate::assert_character(product_name, len = 1)
  checkmate::assert_choice(product_name, mr_list()$data_product)

  # Config
  info <- mr_list() %>%
    dplyr::filter(data_product == product_name)

  # Perform request
  out <- mr_init_wfs_client(silent = TRUE)$
    getFeatures(info$id, ...)

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

  out %>%
    tibble::as_tibble() %>%
    sf::st_as_sf()


}


#' Get the names of the columns and data type of the data product
#'
#' @param product_name identifier of the data product. See [mr_list()]
#'
#' @details
#'   This function becomes useful to write CQL or OGC filters that you can pass to [mr_get()] or [mr_view()] as
#'   it allows you to know the column names and the data types beforehand. Use it together with [mr_unique()] to
#'   know all the possible values in the column name that you want to query on.
#'
#'   The actual description of each column is available only to the Maritime Boundaries products.
#'   See \url{https://marineregions.org/eezattribute.php}
#'
#' @return A data frame with the column names and data type in the Marine Regions data product
#' @export
#'
#' @examples
#' mr_colnames("eez")
#' mr_colnames("ecoregions")
mr_colnames <- function(product_name){

  checkmate::assert_character(product_name, len = 1)
  checkmate::assert_choice(product_name, mr_list()$data_product)

  # Config
  info <- mr_list() %>%
    dplyr::filter(data_product == product_name)

  # Perform
  mr_init_wfs_client(silent = TRUE)$
    getCapabilities()$
    findFeatureTypeByName(info$id)$
    getDescription(pretty = TRUE) %>%
    dplyr::transmute(data_product = product_name, column_name = name, type)

}


#' Get all the possible values of a column of a Marine Regions data product
#'
#' @param product_name identifier of the data product. See [mr_list()]
#' @param colname column name in the data product. See [mr_colnames()]
#'
#' @details
#' This function becomes useful to write CQL or OGC filters that you can pass to [mr_get()] or [mr_view()] as
#' it helps to know all the possible values in the column name that you want to query on beforehand. Use it
#' together with [mr_colnames()] to know the columns and data types in the data product.
#'
#' # Geometry columns
#' Note that columns of type `geometry` are forbidden as their performance is sub-optimal and would likely
#' crash your R session.
#'
#' @return A numeric or character vector with the unique values of a column of a Marine Regions data product.
#' @export
#'
#' @examples
#' mr_unique("ecs", "pol_type")
#' mr_unique("ecs_boundaries", "line_type")
mr_unique <- function(product_name, colname){

  checkmate::assert_character(product_name, len = 1)
  checkmate::assert_choice(product_name, mr_list()$data_product)

  colnames <- mr_colnames(product_name)
  checkmate::assert_choice(colname, colnames[, 2])

  # Geometry column not allowed
  datatype <- tolower(subset(colnames[, 3], colnames[, 2] == colname))
  if(datatype == "geometry"){ stop("`colname` must not be of type geometry") }

  # Config
  info <- mr_list() %>%
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

#' @rdname mr_unique
#' @export
mr_distinct <- mr_unique


.mr_init_wfs_client <- function(version = "2.0.0", silent = FALSE, ...){
  checkmate::assert_character(version)
  checkmate::assert_choice(version, c("1.0.0", "1.1.1", "2.0.0"))

  # Perform
  url <- "https://geo.vliz.be/geoserver/wfs"

  try({
    wfs <- ows4R::WFSClient$new(url, version, ...)
  }, silent = TRUE)

  if(exists("wfs")){
    if(!silent){
      cli::cli_alert_success("WFS client created successfully")
      cli::cli_alert_info("Service: {.val {url}}")
      cli::cli_alert_info("Version: {.val {version}}")
    }

    return(wfs)
  }

  # Handle exceptions
  msg <- c("x" = "WFS client creation failed")


  if(!curl::has_internet()){
    cli::cli_abort(c(
      msg,
      "!" = "{.code curl::has_internet()} is {.val FALSE}",
      "i" = "Did you check your internet connection?"
    ))
  }

  resp <- httr2::request(paste0(url, "?request=GetCapabilities")) %>%
    httr2::req_method("HEAD") %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  if(httr2::resp_is_error(resp)){
    cli::cli_abort(c(
      msg,
      "!" = "HTTP {paste0(httr2::resp_status(resp), ' ', httr2::resp_status_desc(resp))}",
      "i" = "Service: {.val {url}}"
    ))
  }


  cli::cli_abort(c(
    msg,
    "!" = "An exception has occurred. Please raise an issue in {.val {packageDescription('mregions2')$BugReports}}"
  ))

}

#' Creates WFS client in Marine Regions
#'
#' @param version The WFS version. Supported: c("1.0.0", "1.1.1", "2.0.0")
#'
#' @return an object of class c("WFSClient", "OWSClient", "OGCAbstractObject","R6"). See package ows4R.
#' @export
#'
#' @examples
#' wfs <- mr_init_wfs_client()
mr_init_wfs_client <- memoise::memoise(.mr_init_wfs_client)
