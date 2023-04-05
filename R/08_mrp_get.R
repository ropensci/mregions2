#' Get a data product
#'
#' @param layer (character) Identifier of the data product. See [mrp_list]
#' @param cql_filter (character) Contextual Query Language (CQL) filter. See details.
#' @param filter (character) Standard OGC filter specification. See details.
#' @param count (numeric) Maximum number of features to be retrieved.
#'
#' @details
#'   This function uses [WMS services](https://en.wikipedia.org/wiki/Web_Map_Service) to load quickly
#'   a Leaflet viewer of a Marine Regions data product. It uses [EMODnet Bathymetry](https://emodnet.ec.europa.eu) as background layer.
#'
#'
#'   ## Filters
#'
#'   Both the [Contextual Query Language (CQL) filter](https://portal.ogc.org/files/96288) and the [standard OGC filter specification](https://www.ogc.org/standards/filter) allow to
#'   query the server before performing a request. This will boost performance as you will only retrieve the area of your interest. It is possible to query on attributes, but also perform
#'   geospatial queries. For instance, you can query a bounding box of interest.
#'
#'   CQL filters are possible only in geoserver. Marine Regions uses a geoserver instance to serve its data products.
#'   A tutorial on CQL filters is available in the [geoserver web site](https://docs.geoserver.org/stable/en/user/tutorials/cql/cql_tutorial.html).
#'
#'
#' @return An sf object with the Marine Regions data product
#' @export
#'
#' @seealso [mrp_list] to describe the list of products, [mrp_view()] to visualize the data product in advance, [mrp_colnames()] and [mrp_col_unique()] to get the name, data type and unique values of a the columns of a data product, useful to query
#' with the arguments `cql_filter` or `filter`
#'
#' @examples
#' \dontrun{
#' # See the list of all data products
#' View(mrp_list)
#'
#' # We want the Exclusive Economic Zones of Portugal. Let's first visualize the product:
#' mrp_view("eez")
#'
#' # See all the columns on this data product
#' mrp_colnames("eez")
#'
#' # We should query on sovereign
#' # See all the possible values of sovereign1, sovereign2 and sovereign3
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
#' # Portugal is only in sovereign1. Let's write a CQL filter to get only
#' # the EEZs of Portugal, or those where Portugal is a party of a dispute or a joint regime
#' portugal_eez <- mrp_get("eez", cql_filter = "sovereign1 = 'Portugal'")
#'
#' # You can also limit the number of features to be requested
#' mrp_get("eez", count = 5)
#' }
mrp_get <- function(layer, cql_filter = NULL, filter = NULL, count = NULL){

  # Assertions
  checkmate::assert_character(layer, len = 1)
  checkmate::assert_choice(layer, mrp_list$layer)
  checkmate::assert_character(cql_filter, null.ok = TRUE, len = 1)
  checkmate::assert_character(filter, null.ok = TRUE, len = 1)
  assert_only_one_filter(cql_filter, filter)
  count <- checkmate::assert_integerish(count, lower = 1, len = 1,
                                        coerce = TRUE, null.ok = TRUE)
  assert_internet()

  # Config
  namespace <- subset(mrp_list$namespace, mrp_list$layer == layer)
  url <- httr2::url_parse("https://geo.vliz.be/geoserver/ows")
  url$query <- list(service = "wfs",
                    version = "2.0.0",
                    request = "GetFeature",
                    typeName = glue::glue("{namespace}:{layer}"),
                    cql_filter = cql_filter,
                    filter = filter,
                    count = count,
                    outputFormat = "text/csv")

  # Perform
  resp <- httr2::url_build(url) %>%
    httr2::request() %>%
    httr2::req_user_agent(mr_user_agent) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  # If something wrong, e.g. wrong filter, raise
  if(httr2::resp_is_error(resp)){

    status <- httr2::resp_status(resp)
    desc <- httr2::resp_status_desc(resp)

    msg <- c("!" = "HTTP {status} {desc}",
             "i" = "Layer: {.val {layer}}")

    try({
      exception <- httr2::resp_body_xml(resp) %>%
        xml2::xml_find_all(glue::glue("//ows:Exception"))

      exception_code <- exception %>%
        xml2::xml_attr("exceptionCode")

      exception_text <- exception %>%
        xml2::xml_find_all(glue::glue("//ows:ExceptionText")) %>%
        xml2::xml_text()

      msg <- c(msg,
               "i" = "Exception Code: {.emph {exception_code}}",
               "i" = "Exception text: {.emph {exception_text}}"
               )

    })
    cli::cli_abort(msg)
  }

  # Continue if all ok
  resp <- resp %>%
    httr2::resp_body_string(encoding = "UTF-8") %>%
    textConnection() %>%
    utils::read.csv(stringsAsFactors = FALSE, fileEncoding = "UTF-8")

  attr(resp, "class") <- c("tbl_df", "tbl", "data.frame")

  out <- sf::st_as_sf(resp, wkt = "the_geom", crs = 4326)
  out$FID <- NULL

  mrp_list <- NULL

  out
}




.mrp_colnames <- function(layer){

  checkmate::assert_character(layer, len = 1)
  checkmate::assert_choice(layer, mrp_list$layer)

  # Config
  namespace <- subset(mrp_list$namespace, mrp_list$layer == layer)
  url <- httr2::url_parse("https://geo.vliz.be/geoserver/ows")
  url$query <- list(service = "wfs",
                    version = "2.0.0",
                    request = "DescribeFeatureType",
                    typeName = glue::glue("{namespace}:{layer}")
                    )

  # Perform
  request <- httr2::url_build(url) %>%
    httr2::request() %>%
    httr2::req_user_agent(mr_user_agent) %>%
    httr2::req_perform()

  resp <- request %>%
    httr2::resp_body_xml() %>%
    xml2::xml_find_all("//xsd:element")

  out <- data.frame(
    layer = layer,
    colname = xml2::xml_attr(resp, "name"),
    type = gsub("xsd:", "", xml2::xml_attr(resp, "type")),
    stringsAsFactors = FALSE
  )

  out <- subset(out, out$colname != "the_geom")
  out <- subset(out, out$colname != layer)

  attr(out, "class") <- c("tbl_df", "tbl", "data.frame")

  mrp_list <- NULL

  out

}
#' Get the names of the columns and data type of the data product
#'
#' @param layer (character) Identifier of the data product. See [mrp_list]
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
#' @seealso [mrp_list] to describe the list of products, [mrp_col_unique()] to get the unique values of a the
#' columns of a data product, useful to write queries that can be passed to [mrp_get()] or [mrp_view()] via the
#' arguments `cql_filter` or `filter`.
#'
#' @examples
#' mrp_colnames("eez")
#' mrp_colnames("ecoregions")
mrp_colnames <- memoise::memoise(.mrp_colnames)



.mrp_col_unique <- function(layer, colname){

  checkmate::assert_character(layer, len = 1)
  checkmate::assert_choice(layer, mrp_list$layer)

  checkmate::assert_character(colname, len = 1)
  column_names <- mrp_colnames(layer)
  checkmate::assert_choice(colname, column_names$colname)


  # Config
  namespace <- subset(mrp_list$namespace, mrp_list$layer == layer)
  url <- httr2::url_parse("https://geo.vliz.be/geoserver/ows")
  url$query <- list(service = "wfs",
                    version = "2.0.0",
                    request = "GetPropertyValue",
                    typeNames = glue::glue("{namespace}:{layer}"),
                    valueReference = colname
  )

  # Perform
  resp <- httr2::url_build(url) %>%
    httr2::request() %>%
    httr2::req_user_agent(mr_user_agent) %>%
    httr2::req_perform() %>%
    httr2::resp_body_xml() %>%
    xml2::xml_find_all(glue::glue("//wfs:member")) %>%
    xml2::xml_text() %>%
    unique()

  datatype <- tolower(subset(column_names$type, column_names$colname == colname))
  if(datatype %in% c("numeric", "int", "double")) resp <- resp %>% as.numeric()
  if(datatype %in% c("date")) resp <- resp %>% as.Date()
  if(datatype %in% c("timestamp")) resp <- resp %>% as.POSIXct(tz = "UTC")

  mrp_list <- NULL

  sort(resp)
}
#' Get all the possible values of a column of a Marine Regions data product
#'
#' @param layer (character) Identifier of the data product. See [mrp_list]
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
#' @seealso [mrp_list] to describe the list of products, [mrp_colnames()] to get the names and data type of
#' the columns of a data product, useful to write queries that can be passed to [mrp_get()] or [mrp_view()] via
#' the arguments `cql_filter` or `filter`.
#'
#' @examples
#' mrp_col_unique("ecs", "pol_type")
#' mrp_col_unique("ecs_boundaries", "line_type")
mrp_col_unique <- memoise::memoise(.mrp_col_unique)

#' @rdname mrp_col_unique
#' @export
mrp_col_distinct <- mrp_col_unique



