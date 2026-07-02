# Package index

## Marine Regions Gazetteer

Functions to access the Marine Regions Gazetteer

- [`MRGID`](https://docs.ropensci.org/mregions2/reference/MRGID.md) :
  Marine Regions Global Identifier or MRGID (Documentation)
- [`gaz_search()`](https://docs.ropensci.org/mregions2/reference/gaz_search.md)
  : Search in the Marine Regions Gazetteer by names, MRGID or reverse
  geocode with a pair of WGS84 coordinates x and y
- [`gaz_search_by_type()`](https://docs.ropensci.org/mregions2/reference/gaz_search_by_type.md)
  : Retrieve Gazetteer Records by Placetype
- [`gaz_search_by_source()`](https://docs.ropensci.org/mregions2/reference/gaz_search_by_source.md)
  : Retrieve Gazetteer Records by Source
- [`gaz_types()`](https://docs.ropensci.org/mregions2/reference/gaz_types.md)
  : Get all the place types of the Marine Regions Gazetteer
- [`gaz_sources()`](https://docs.ropensci.org/mregions2/reference/gaz_sources.md)
  : Get all the Marine Regions sources
- [`gaz_geometry()`](https://docs.ropensci.org/mregions2/reference/gaz_geometry.md)
  : Get the geometries of a Marine Regions Geo-Object
- [`gaz_relations()`](https://docs.ropensci.org/mregions2/reference/gaz_relations.md)
  : Walk the hierarchy of the MarineRegions Gazetter given a Gazetteer
  MRGID or Gazetteer entries

## Marine Regions Data Products

List, visualize and download the Marine Regions Data Products, such as
the Maritime Boundaries

- [`mrp_list`](https://docs.ropensci.org/mregions2/reference/mrp_list.md)
  : Available data products at Marine Regions
- [`mrp_ontology`](https://docs.ropensci.org/mregions2/reference/mrp_ontology.md)
  : Marine Regions Data Products Ontology
- [`mrp_view()`](https://docs.ropensci.org/mregions2/reference/mrp_view.md)
  [`mrp_view_eez()`](https://docs.ropensci.org/mregions2/reference/mrp_view.md)
  [`mrp_view_eez_boundaries()`](https://docs.ropensci.org/mregions2/reference/mrp_view.md)
  [`mrp_view_eez_12nm()`](https://docs.ropensci.org/mregions2/reference/mrp_view.md)
  [`mrp_view_eez_24nm()`](https://docs.ropensci.org/mregions2/reference/mrp_view.md)
  [`mrp_view_eez_internal_waters()`](https://docs.ropensci.org/mregions2/reference/mrp_view.md)
  [`mrp_view_eez_archipelagic_waters()`](https://docs.ropensci.org/mregions2/reference/mrp_view.md)
  [`mrp_view_high_seas()`](https://docs.ropensci.org/mregions2/reference/mrp_view.md)
  [`mrp_view_ecs()`](https://docs.ropensci.org/mregions2/reference/mrp_view.md)
  [`mrp_view_ecs_boundaries()`](https://docs.ropensci.org/mregions2/reference/mrp_view.md)
  [`mrp_view_iho()`](https://docs.ropensci.org/mregions2/reference/mrp_view.md)
  [`mrp_view_goas()`](https://docs.ropensci.org/mregions2/reference/mrp_view.md)
  [`mrp_view_eez_iho()`](https://docs.ropensci.org/mregions2/reference/mrp_view.md)
  [`mrp_view_eez_land()`](https://docs.ropensci.org/mregions2/reference/mrp_view.md)
  [`mrp_view_longhurst()`](https://docs.ropensci.org/mregions2/reference/mrp_view.md)
  [`mrp_view_cds()`](https://docs.ropensci.org/mregions2/reference/mrp_view.md)
  [`mrp_view_eca_reg13_nox()`](https://docs.ropensci.org/mregions2/reference/mrp_view.md)
  [`mrp_view_eca_reg14_sox_pm()`](https://docs.ropensci.org/mregions2/reference/mrp_view.md)
  [`mrp_view_worldheritagemarineprogramme()`](https://docs.ropensci.org/mregions2/reference/mrp_view.md)
  [`mrp_view_lme()`](https://docs.ropensci.org/mregions2/reference/mrp_view.md)
  [`mrp_view_ecoregions()`](https://docs.ropensci.org/mregions2/reference/mrp_view.md)
  [`mrp_view_seavox_v18()`](https://docs.ropensci.org/mregions2/reference/mrp_view.md)
  : Visualize a Marine Regions data product without downloading.
- [`mrp_get()`](https://docs.ropensci.org/mregions2/reference/mrp_get.md)
  : Get a data product
- [`mrp_colnames()`](https://docs.ropensci.org/mregions2/reference/mrp_colnames.md)
  : Get the names of the columns and data type of the data product
- [`mrp_col_unique()`](https://docs.ropensci.org/mregions2/reference/mrp_col_unique.md)
  [`mrp_col_distinct()`](https://docs.ropensci.org/mregions2/reference/mrp_col_unique.md)
  : Get all the possible values of a column of a Marine Regions data
  product

## RESTful services

Low-level functions that interact with the Marine Regions Gazetteer REST
web services

- [`gaz_rest`](https://docs.ropensci.org/mregions2/reference/gaz_rest.md)
  : Marine Regions Gazetteer RESTful services (Documentation)
- [`gaz_rest_records_by_name()`](https://docs.ropensci.org/mregions2/reference/gaz_rest_records_by_name.md)
  : Get Gazetteer Records for a given name
- [`gaz_rest_records_by_names()`](https://docs.ropensci.org/mregions2/reference/gaz_rest_records_by_names.md)
  : Get Gazetteer Records for all given names
- [`gaz_rest_record_by_mrgid()`](https://docs.ropensci.org/mregions2/reference/gaz_rest_record_by_mrgid.md)
  : Get one record for the given MRGID
- [`gaz_rest_records_by_lat_long()`](https://docs.ropensci.org/mregions2/reference/gaz_rest_records_by_lat_long.md)
  : Get all gazetteer records where the geometry intersects with the
  given latitude and longitude
- [`gaz_rest_records_by_type()`](https://docs.ropensci.org/mregions2/reference/gaz_rest_records_by_type.md)
  : Retrieve Gazetteer Records by Placetype
- [`gaz_rest_records_by_source()`](https://docs.ropensci.org/mregions2/reference/gaz_rest_records_by_source.md)
  : Retrieve Gazetteer Records by Source
- [`gaz_rest_relations_by_mrgid()`](https://docs.ropensci.org/mregions2/reference/gaz_rest_relations_by_mrgid.md)
  : Retrieve Gazetter Relations by MRGID
- [`gaz_rest_source_by_sourceid()`](https://docs.ropensci.org/mregions2/reference/gaz_rest_source_by_sourceid.md)
  : Get the name of a source by providing a sourceID
- [`gaz_rest_types()`](https://docs.ropensci.org/mregions2/reference/gaz_rest_types.md)
  : Get all the place types of the Marine Regions Gazetteer
- [`gaz_rest_sources()`](https://docs.ropensci.org/mregions2/reference/gaz_rest_sources.md)
  : Get all the Marine Regions sources
- [`gaz_rest_geometries()`](https://docs.ropensci.org/mregions2/reference/gaz_rest_geometries.md)
  : Get the geometries associated with a gazetteer record
- [`gaz_rest_names_by_mrgid()`](https://docs.ropensci.org/mregions2/reference/gaz_rest_names_by_mrgid.md)
  : Get the names for a given MRGID
- [`gaz_rest_wmses()`](https://docs.ropensci.org/mregions2/reference/gaz_rest_wmses.md)
  : Get WMS information for a given MRGID
