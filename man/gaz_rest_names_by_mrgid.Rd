% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{gaz_rest_names_by_mrgid}
\alias{gaz_rest_names_by_mrgid}
\title{Get the names for a given MRGID}
\usage{
gaz_rest_names_by_mrgid(mrgid)
}
\arguments{
\item{mrgid}{(integer) A valid Marine Regions Gazetteer Identifier (\link{MRGID})}
}
\value{
a vector with all the names of a Marine Regions Gazetteer entry
}
\description{
Get the names for a given MRGID
}
\examples{
gaz_rest_names_by_mrgid(3293)
gaz_rest_names_by_mrgid(14)
}
\seealso{
\link{gaz_rest}, \link{MRGID}
}
