% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/08_mrp_get.R
\name{mrp_colnames}
\alias{mrp_colnames}
\title{Get the names of the columns and data type of the data product}
\usage{
mrp_colnames(layer)
}
\arguments{
\item{layer}{(character) Identifier of the data product. See \link{mrp_list}}
}
\value{
A data frame with the column names and data type in the Marine Regions data product
}
\description{
Get the names of the columns and data type of the data product
}
\details{
This function becomes useful to write CQL or OGC filters that you can pass to \code{\link[=mrp_get]{mrp_get()}} or \code{\link[=mrp_view]{mrp_view()}} as
it allows you to know the column names and the data types beforehand. Use it together with \code{\link[=mrp_col_unique]{mrp_col_unique()}} to
know all the possible values in the column name that you want to query on.

The actual description of each column is available only to the Maritime Boundaries products.
See \url{https://marineregions.org/eezattribute.php}
}
\examples{
\donttest{
mrp_colnames("eez")
mrp_colnames("ecoregions")
}
}
\seealso{
\link{mrp_list} to describe the list of products, \code{\link[=mrp_col_unique]{mrp_col_unique()}} to get the unique values of a the
columns of a data product, useful to write queries that can be passed to \code{\link[=mrp_get]{mrp_get()}} or \code{\link[=mrp_view]{mrp_view()}} via the
arguments \code{cql_filter} or \code{filter}.
}
