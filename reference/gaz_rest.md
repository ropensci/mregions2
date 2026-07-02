# Marine Regions Gazetteer RESTful services (Documentation)

*RESTful service* REST (REpresentational State Transfer) is a simple
stateless architecture that generally runs over HTTP.

[mregions2](https://docs.ropensci.org/mregions2/reference/mregions2-package.md)
makes use of the [RESTful
API](https://www.marineregions.org/gazetteer.php?p=webservices) created
and maintained by Marine Regions. The functions with names starting as
`gaz_rest_*` perform [HTTP requests](https://httr2.r-lib.org/) to read
the Marine Regions REST API. They are closer to the definition of each
function in the Marine Regions REST API. All the gazetteer functions
such as
[`gaz_search()`](https://docs.ropensci.org/mregions2/reference/gaz_search.md)or
[`gaz_relations()`](https://docs.ropensci.org/mregions2/reference/gaz_relations.md)
make use of these `gaz_rest_*` functions.

## Value

Returns a help page: `gaz_rest` is not a function but documentation.

## See also

[`gaz_rest_geometries()`](https://docs.ropensci.org/mregions2/reference/gaz_rest_geometries.md),
[`gaz_rest_names_by_mrgid()`](https://docs.ropensci.org/mregions2/reference/gaz_rest_names_by_mrgid.md),
[`gaz_rest_record_by_mrgid()`](https://docs.ropensci.org/mregions2/reference/gaz_rest_record_by_mrgid.md),
[`gaz_rest_records_by_lat_long()`](https://docs.ropensci.org/mregions2/reference/gaz_rest_records_by_lat_long.md),
[`gaz_rest_records_by_name()`](https://docs.ropensci.org/mregions2/reference/gaz_rest_records_by_name.md),
[`gaz_rest_records_by_names()`](https://docs.ropensci.org/mregions2/reference/gaz_rest_records_by_names.md),
[`gaz_rest_records_by_source()`](https://docs.ropensci.org/mregions2/reference/gaz_rest_records_by_source.md),
[`gaz_rest_records_by_type()`](https://docs.ropensci.org/mregions2/reference/gaz_rest_records_by_type.md),
[`gaz_rest_relations_by_mrgid()`](https://docs.ropensci.org/mregions2/reference/gaz_rest_relations_by_mrgid.md),
[`gaz_rest_sources()`](https://docs.ropensci.org/mregions2/reference/gaz_rest_sources.md),
[`gaz_rest_source_by_sourceid()`](https://docs.ropensci.org/mregions2/reference/gaz_rest_source_by_sourceid.md),
[`gaz_rest_types()`](https://docs.ropensci.org/mregions2/reference/gaz_rest_types.md),
[`gaz_rest_wmses()`](https://docs.ropensci.org/mregions2/reference/gaz_rest_wmses.md)

## Examples

``` r
?gaz_rest
```
