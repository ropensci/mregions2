#' Get a data product
#'
#' @param layer (character) Identifier of the data product. See [mrp_list]
#' @param path (character) Path to save the requests. Default is [base::tempdir()]. See details.
#' @param cql_filter (character) Contextual Query Language (CQL) filter. See details.
#' @param filter (character) Standard OGC filter specification. See details.
#' @param count (numeric) Maximum number of features to be retrieved.
#'
#' @details
#'   This function uses [WFS services](https://en.wikipedia.org/wiki/Web_Map_Service) to download the
#'   Marine Regions layers as ESRI Shapefiles.
#'
#'   ## Caching
#'   By default, the layers are downloaded to a temporal directory ([base::tempdir()]). You can provide
#'   a path in the `path` argument. But you can also set a path with `# options("mregions2.download_path" = "my/path/")`.
#'
#'   Because it is possible to add filters, each request is identified with a crc32 hash, provided with
#'   [digest::digest()] and attached to the file downloaded.
#'
#'   Once a layer is downloaded, it will be read from the cache during the next two weeks. To avoid this, simply
#'   delete the layers in the cache path.
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
#'
#' @return An sf object with the Marine Regions data product
#' @export
#'
#' @seealso [mrp_list] to describe the list of products, [mrp_view()] to visualize the data product in advance, [mrp_colnames()] and [mrp_col_unique()] to get the name, data type and unique values of a the columns of a data product, useful to query
#' with the arguments `cql_filter` or `filter`
#'
#' @examples \donttest{
#' # Set cache path. Default is a temporal directory
#' options(mregions2.download_path = tempdir())
#'
#' getOption("mregions2.download_path")
#' #> [1] "/tmp/RtmpARLgoE"
#'
#' # See the list of all data products
#' mrp_list
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
#' # If you perform this request again, it will be read from the cache instead
#' portugal_eez <- mrp_get("eez", cql_filter = "sovereign1 = 'Portugal'")
#' #> Cache is fresh. Reading: /tmp/RtmpARLgoE/eez-1951c8b7/eez.shp
#' #> (Last Modified: 2023-04-24 17:45:16)
#'
#' # You can also limit the number of features to be requested
#' mrp_get("eez", count = 5)
#' }
mrp_get <- function(layer, path = getOption("mregions2.download_path", tempdir()), cql_filter = NULL, filter = NULL, count = NULL){

  # Assertions
  checkmate::assert_character(layer, len = 1)
  checkmate::assert_choice(layer, mregions2::mrp_list$layer)
  checkmate::assert_character(cql_filter, null.ok = TRUE, len = 1)
  checkmate::assert_character(filter, null.ok = TRUE, len = 1)
  assert_only_one_filter(cql_filter, filter)
  count <- checkmate::assert_integerish(count, lower = 1, len = 1,
                                        coerce = TRUE, null.ok = TRUE)
  stopifnot(dir.exists(path))
  assert_internet()


  # Config
  namespace <- subset(mregions2::mrp_list$namespace, mregions2::mrp_list$layer == layer)
  url <- httr2::url_parse("https://geo.vliz.be/geoserver/ows")
  url$query <- list(service = "wfs",
                    version = "2.0.0",
                    request = "GetFeature",
                    srsName = "EPSG:4326",
                    typeName = glue::glue("{namespace}:{layer}"),
                    cql_filter = cql_filter,
                    filter = filter,
                    count = count,
                    outputFormat = "SHAPE-ZIP")
  url <- httr2::url_build(url)

  # Cache
  hash <- digest::digest(url, algo = "crc32")
  hash <- glue::glue('{layer}-{hash}')
  cached_zip_path <- file.path(path, glue::glue('{hash}.zip'))
  cached_unzip_path <- file.path(path, hash)
  cached_file_path <- file.path(cached_unzip_path, glue::glue('{layer}.shp'))

  do_request <- TRUE

  if(file.exists(cached_file_path)) {
    cached_file_time <- file.info(cached_file_path)$ctime

    cached_file_is_fresh <- difftime(
      Sys.time(), cached_file_time, units = "weeks"
    ) %>% as.numeric() %>% `<`(cache_max_time())

    if(cached_file_is_fresh){
      do_request <- FALSE
      cli::cli_text("Cache is fresh. Reading: {.path {cached_file_path}}")
      cli::cli_text("(Last Modified: {.emph {cached_file_time}})")
    }
  }

  # Perform
  if(do_request){
    resp <- httr2::request(url) %>%
      httr2::req_user_agent(mr_user_agent) %>%
      httr2::req_error(is_error = function(resp) FALSE) %>%
      httr2::req_perform(path = cached_zip_path) %>%
      mrp_get_sanity_check()

    dir.create(cached_unzip_path, showWarnings = FALSE)
    utils::unzip(zipfile = cached_zip_path, exdir = cached_unzip_path, overwrite = TRUE)

    if(!is_test()) try_clean_up(cached_zip_path)
  }

  check_server_warning(cached_unzip_path)

  mrp_list <- NULL # Avoid R CMD Check note

  out <- sf::st_read(cached_file_path, quiet = TRUE, stringsAsFactors = FALSE)
  attr(out, "class") <- c("sf", "tbl_df", "tbl", "data.frame")
  out
}

cache_max_time <- function(){
  weeks <- Sys.getenv("TESTPKG.CACHETIME", 4)
  weeks
}

mrp_get_sanity_check <- function(resp){
  is_error <- httr2::resp_is_error(resp)
  if(is_error){
    status <- httr2::resp_status(resp)
    desc <- httr2::resp_status_desc(resp)

    msg <- c("!" = "HTTP {status} {desc}")
    try({
      exception <- httr2::resp_body_string(resp) %>%
        xml2::read_xml() %>%
        xml2::xml_find_all(glue::glue("//ows:Exception"))

      exception_code <- exception %>%
        xml2::xml_attr("exceptionCode")

      exception_text <- exception %>%
        xml2::xml_find_all(glue::glue("//ows:ExceptionText")) %>%
        xml2::xml_text()

      msg <- c(msg,
               "i" = "Exception Code: {.emph {exception_code}}",
               "i" = "Exception Text: {.emph {exception_text}}"
      )

      if(!is_test()) try_clean_up(resp$body)
    })
    cli::cli_abort(msg)
  }

  resp
}

try_clean_up <- function(path) try({file.remove(path)}, silent = TRUE)

check_server_warning <- function(cached_unzip_path){
  readme <- file.path(cached_unzip_path, "README.TXT")

  if(file.exists(readme)){
    msg <- readLines(readme, warn = FALSE, skipNul = TRUE)
    msg <- paste0(msg, collapse = "; ")
    warning(msg, call. = FALSE)
  }

  invisible(NULL)
}

.mrp_colnames <- function(layer){

  checkmate::assert_character(layer, len = 1)
  checkmate::assert_choice(layer, mregions2::mrp_list$layer)

  # Config
  namespace <- subset(mregions2::mrp_list$namespace, mregions2::mrp_list$layer == layer)
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
#' @examples \donttest{
#' mrp_colnames("eez")
#' mrp_colnames("ecoregions")
#' }
mrp_colnames <- memoise::memoise(.mrp_colnames)



.mrp_col_unique <- function(layer, colname){

  checkmate::assert_character(layer, len = 1)
  checkmate::assert_choice(layer, mregions2::mrp_list$layer)

  checkmate::assert_character(colname, len = 1)
  column_names <- mrp_colnames(layer)
  checkmate::assert_choice(colname, column_names$colname)


  # Config
  namespace <- subset(mregions2::mrp_list$namespace, mregions2::mrp_list$layer == layer)
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

  datatype <- tolower(subset(
    column_names$type,
    column_names$colname == colname
  ))
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
#' @examples \donttest{
#' mrp_col_unique("ecs", "pol_type")
#' mrp_col_unique("ecs_boundaries", "line_type")
#' }
mrp_col_unique <- memoise::memoise(.mrp_col_unique)

#' @rdname mrp_col_unique
#' @export
mrp_col_distinct <- mrp_col_unique



