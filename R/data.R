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
#'   \item{imis}{Url of the data products in the [Integrated Marine Information System (IMIS)](https://vliz.be/en/imis)}
#'   \item{abstract}{Description of the data product}
#' }
#'
#' @source <https://marineregions.org/sources.php> <https://marineregions.org/eezmethodology.php>
#'
#' @examples
#' mrp_list
"mrp_list"

#' Marine Regions Data Products Ontology
#'
#' More information available at `vignette("mrp_ontology", package = "mregions2")`
#'
#' @format ## `mrp_ontology`
#' A data frame with 374 rows and 4 columns:
#' \describe{
#'   \item{layer}{Identifier of the data product. Use in [mrp_get()]}
#'   \item{colname}{Name of the columns of each data product.}
#'   \item{type}{Data type of the column.}
#'   \item{definition}{Definition of the column.}
#' }
#'
#' @source <https://marineregions.org/sources.php> <https://marineregions.org/eezattribute.php>
#'
#' @examples
#' mrp_ontology
"mrp_ontology"
