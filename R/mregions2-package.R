#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL


#' @name MRGID
#' @title Marine Regions Global Identifier or MRGID (Documentation)
#'
#' @description
#' Many functions of [mregions2] make use of the argument `mrgid` or return data with the numeric variable `MRGID`.
#'
#' But what is this identifier?
#'
#' This is an unique and persistent identifier of each entry in the Marine Regions Gazetteer. This identifier
#' consists in a URI containing a number, unique for each entry in the Marine Regions Gazetteer. The R package [mregions2]
#' uses this number in its functions, and it should be considered a synonym of the standard definition of MRGID.
#'
#' See the section details for a more in depth definition of the `MRGID`
#'
#' @details
#' From <https://marineregions.org/mrgid.php> :
#'
#' ## Standards
#' Place names change over time, and the same names may be used for different locations. Available
#' gazetteers may find locations of some marine place names, but a truly global standard for marine
#' place names is lacking. Marine Regions tries to establish for the first time a standardized list
#' of georeferenced marine place names and marine areas. In order to preserve the identity of the marine
#' geographic objects from the database, and to name and locate the geographic resources on the web, we
#' promote the Marine Regions Geographic IDentifier, or the MRGID.
#'
#' ## MRGID
#' The Marine Regions Geographic IDentifier is:
#'
#' - *unique* by using a URI (Uniform Resource Identifier), it's unique across the internet.
#'
#'   Syntax `http://marineregions.org/mrgid/<number>`
#'
#' - *persistent* we will never delete, nor change the concept behind an MRGID
#'
#' - *resolvable* pointing your client to an MRGID will return
#'         - the reply of the webservice call `getGazetteerRecordByMRGID`, when using content negotiation
#'           (`text/turtle` or `application/ld+json`)
#'         - the webpage of the MRGID, when using a browser
#'
#' For an identifier to be persistent, it requires the governing body to arrange for the identifier to
#' be available for the long term. Use of the MRGID, as URI and persistent identifier has the commitment
#' of the Flanders Marine Institute, issuing the identifier to maintain the http domain registration, and
#' a strategy for managing the domain and the web servers.
#'
#' @return Returns a help page: `MRGID` is not a function but documentation.
#'
#' @examples \donttest{?MRGID}
NULL


#' @name gaz_rest
#' @title Marine Regions Gazetteer RESTful services (Documentation)
#'
#' @description
#'
#' _RESTful service_
#' REST (REpresentational State Transfer) is a simple stateless architecture
#' that generally runs over HTTP.
#'
#'
#' [mregions2] makes use of the [RESTful API](https://www.marineregions.org/gazetteer.php?p=webservices) created and maintained by
#' Marine Regions. The functions with names starting as `gaz_rest_*` perform
#' [HTTP requests](https://httr2.r-lib.org/) to read the Marine Regions REST API.
#' They are closer to the definition of each function in the Marine Regions
#' REST API. All the gazetteer functions such as `gaz_search()`or
#' `gaz_relations()` make use of these `gaz_rest_*` functions.
#'
#' @seealso [gaz_rest_geometries()], [gaz_rest_names_by_mrgid()],
#' [gaz_rest_record_by_mrgid()], [gaz_rest_records_by_lat_long()],
#' [gaz_rest_records_by_name()], [gaz_rest_records_by_names()],
#' [gaz_rest_records_by_source()], [gaz_rest_records_by_type()],
#' [gaz_rest_relations_by_mrgid()], [gaz_rest_sources()],
#' [gaz_rest_source_by_sourceid()], [gaz_rest_types()], [gaz_rest_wmses()]
#'
#' @return Returns a help page: `gaz_rest` is not a function but documentation.
#'
#' @examples \donttest{?gaz_rest}
NULL
