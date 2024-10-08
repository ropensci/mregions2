% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/01_gaz_search.R
\name{gaz_rest_records_by_name}
\alias{gaz_rest_records_by_name}
\title{Get Gazetteer Records for a given name}
\usage{
gaz_rest_records_by_name(
  name,
  with_geometry = FALSE,
  typeid = NULL,
  language = NULL,
  like = TRUE,
  fuzzy = TRUE
)
}
\arguments{
\item{name}{(character) Term to search in the Marine Regions Gazetteer}

\item{with_geometry}{(logical) Add geometry to the result data frame? Default = FALSE}

\item{typeid}{(numeric) Restrict to one or more placetypeIDs. Retrieve a list of placetypeIDs with \code{\link[=gaz_rest_types]{gaz_rest_types()}}}

\item{language}{(character) Restrict to one language. Provide as a 2 digits ISO-639. See \link[ISOcodes:ISO_639]{ISOcodes::ISO_639_2}.}

\item{like}{(logical) Add a '\%'-sign before and after the name? (SQL LIKE function). Default = TRUE}

\item{fuzzy}{(logical) Use Levenshtein query to find nearest matches? Default = TRUE}
}
\value{
A data frame with Gazetteer entries
}
\description{
Get Gazetteer Records for a given name
}
\examples{
\donttest{
gaz_rest_records_by_name("Belgian Exclusive Economic Zone", with_geometry = TRUE)
gaz_rest_records_by_name("Bélgica", language = "es")
gaz_rest_records_by_name("Belgium", typeid = c(350, 351))
}
}
\seealso{
\link{gaz_rest}, \link{gaz_rest_records_by_name}
}
