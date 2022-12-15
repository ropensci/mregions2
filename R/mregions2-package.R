#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL


#' @name MRGID
#' @title Marine Regions Global Identifier (MRGID)
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
NULL


#' @name gaz_rest
#' @title Marine Regions Gazetteer RESTful services
#'
#' @description
#'
#' _RESTful service_
#' REST (REpresentational State Transfer) is a simple stateless architecture that generally runs over HTTP.
#'
#' [mregions2] makes use of the RESTful services created and maintained by Marine Regions. You can find more
#' information at the overview of the [Marine Regions Gazetteer web services](https://marineregions.org/gazetteer.php?p=webservices)
NULL
