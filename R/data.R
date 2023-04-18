#' Available data products at Marine Regions
#'
#' A data frame including the name, abstract and
#' some other relevant about each data product in Marine Regions.
#'
#' Direct downloads are also available at: <https://marineregions.org/downloads.php>
#'
#' @format ## `mrp_list`
#' A data frame with 21 rows and 7 columns:
#' \describe{
#'   \item{title}{Data product name}
#'   \item{namespace}{Workspace in geoserver}
#'   \item{layer}{Identifier of the data product. Use in [mrp_get()]}
#'   \item{license}{terms of use of the data products}
#'   \item{citation}{preferred citation of the data products}
#'   \item{doi}{ISO 26324 [Digital Object Identifier](https://www.doi.org/))}
#'   \item{imis}{Url of the data products in the [Integrated Marine Information System (IMIS)](https://vliz.be/imis?)}
#'   \item{abstract}{Description of the data product}
#' }
#'
#' @source <https://marineregions.org/sources.php> <https://marineregions.org/eezmethodology.php>
#'
#' @examples
#' mrp_list
"mrp_list"
