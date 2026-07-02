# Visualize a Marine Regions data product without downloading.

Visualize a Marine Regions data product without downloading.

A series of helpers are available to ease the selection of the data
products. Example: instead of running

`mrp_view("eez")`

You can use

`mrp_view_eez()`

Try `mrp_view_*()`with the identifier of the data product (see
[mrp_list](https://docs.ropensci.org/mregions2/reference/mrp_list.md))

## Usage

``` r
mrp_view(layer, cql_filter = NULL, filter = NULL)

mrp_view_eez(...)

mrp_view_eez_boundaries(...)

mrp_view_eez_12nm(...)

mrp_view_eez_24nm(...)

mrp_view_eez_internal_waters(...)

mrp_view_eez_archipelagic_waters(...)

mrp_view_high_seas(...)

mrp_view_ecs(...)

mrp_view_ecs_boundaries(...)

mrp_view_iho(...)

mrp_view_goas(...)

mrp_view_eez_iho(...)

mrp_view_eez_land(...)

mrp_view_longhurst(...)

mrp_view_cds(...)

mrp_view_eca_reg13_nox(...)

mrp_view_eca_reg14_sox_pm(...)

mrp_view_worldheritagemarineprogramme(...)

mrp_view_lme(...)

mrp_view_ecoregions(...)

mrp_view_seavox_v18(...)
```

## Arguments

- layer:

  (character) Identifier of the data product. See
  [mrp_list](https://docs.ropensci.org/mregions2/reference/mrp_list.md)

- cql_filter:

  (character) Contextual Query Language (CQL) filter. See details.

- filter:

  (character) Standard OGC filter specification. See details.

- ...:

  pass the `cql_filter` and `filter` parameters to `mrp_view()` when
  using one of the helpers

## Value

A leaflet map with a data product visualized via WMS

## Details

This function uses [WMS
services](https://en.wikipedia.org/wiki/Web_Map_Service) to load quickly
a Leaflet viewer of a Marine Regions data product. It uses the [EMODnet
Bathymetry](https://emodnet.ec.europa.eu/en) Digital Terrain Model as
background layer.

### Filters

Both the [Contextual Query Language (CQL)
filter](https://portal.ogc.org/files/96288) and the [standard OGC filter
specification](https://www.ogc.org/publications/standard/filter/) allow
to query the server before performing a request. This will boost
performance as you will only retrieve the area of your interest. It is
possible to query on attributes, but also perform geospatial queries.
For instance, you can query a bounding box of interest.

CQL filters are possible only in geoserver. Marine Regions uses a
geoserver instance to serve its data products. A tutorial on CQL filters
is available in the [geoserver web
site](https://docs.geoserver.org/stable/en/user/tutorials/cql/cql_tutorial.html).

## See also

[mrp_list](https://docs.ropensci.org/mregions2/reference/mrp_list.md) to
describe the list of products,
[`mrp_colnames()`](https://docs.ropensci.org/mregions2/reference/mrp_colnames.md)
and
[`mrp_col_unique()`](https://docs.ropensci.org/mregions2/reference/mrp_col_unique.md)
to get the name, data type and unique values of a the columns of a data
product, useful to query with the arguments `cql_filter` or `filter`,
[`mrp_get()`](https://docs.ropensci.org/mregions2/reference/mrp_get.md)
to get the data products as a [simple
feature](https://r-spatial.github.io/sf/) object.

## Examples

``` r
# \donttest{
# You can pass a product name from mrp_list
mrp_view('eez')

{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG4326","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addTiles","args":["https://tiles.emodnet-bathymetry.eu/2020/baselayer/inspire_quad/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"<a href='https://emodnet.ec.europa.eu/en'>EMODnet<\/a>"}]},{"method":"addWMS","args":["https://geo.vliz.be/geoserver/MarineRegions/wms?",null,null,{"styles":"","format":"image/png","transparent":true,"version":"1.1.1","info_format":"text/html","attribution":"<a href='https://marineregions.org/'>Marine Regions<\/a>","layers":"eez","checkempty":false},null]},{"method":"addTiles","args":["https://tiles.emodnet-bathymetry.eu/osm/labels/inspire_quad/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false}]}]},"evals":[],"jsHooks":[]}
# Or use the helper
mrp_view_eez()

{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG4326","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addTiles","args":["https://tiles.emodnet-bathymetry.eu/2020/baselayer/inspire_quad/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"<a href='https://emodnet.ec.europa.eu/en'>EMODnet<\/a>"}]},{"method":"addWMS","args":["https://geo.vliz.be/geoserver/MarineRegions/wms?",null,null,{"styles":"","format":"image/png","transparent":true,"version":"1.1.1","info_format":"text/html","attribution":"<a href='https://marineregions.org/'>Marine Regions<\/a>","layers":"eez","checkempty":false},null]},{"method":"addTiles","args":["https://tiles.emodnet-bathymetry.eu/osm/labels/inspire_quad/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false}]}]},"evals":[],"jsHooks":[]}
# Example: filter a the Ecoregions 'Azores Canaries Madeira' with mrgid 21885
# You can check the names of the columns beforehand with mrp_colnames('ecoregions')
mrp_view_ecoregions(filter = "
  <Filter>
    <PropertyIsEqualTo>
      <PropertyName>ecoregion</PropertyName>
      <Literal>Azores Canaries Madeira</Literal>
    </PropertyIsEqualTo>
  </Filter>
")

{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG4326","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addTiles","args":["https://tiles.emodnet-bathymetry.eu/2020/baselayer/inspire_quad/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"<a href='https://emodnet.ec.europa.eu/en'>EMODnet<\/a>"}]},{"method":"addWMS","args":["https://geo.vliz.be/geoserver/Ecoregions/wms?filter=%0A%20%20%3CFilter%3E%0A%20%20%20%20%3CPropertyIsEqualTo%3E%0A%20%20%20%20%20%20%3CPropertyName%3Eecoregion%3C/PropertyName%3E%0A%20%20%20%20%20%20%3CLiteral%3EAzores%20Canaries%20Madeira%3C/Literal%3E%0A%20%20%20%20%3C/PropertyIsEqualTo%3E%0A%20%20%3C/Filter%3E%0A",null,null,{"styles":"","format":"image/png","transparent":true,"version":"1.1.1","info_format":"text/html","attribution":"<a href='https://marineregions.org/'>Marine Regions<\/a>","layers":"ecoregions","checkempty":false},null]},{"method":"addTiles","args":["https://tiles.emodnet-bathymetry.eu/osm/labels/inspire_quad/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false}]}]},"evals":[],"jsHooks":[]}
# OGC filter are very verbose... but luckily you can use a CQL filter instead
mrp_view_ecoregions(cql_filter = "ecoregion = 'Azores Canaries Madeira'")

{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG4326","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addTiles","args":["https://tiles.emodnet-bathymetry.eu/2020/baselayer/inspire_quad/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"<a href='https://emodnet.ec.europa.eu/en'>EMODnet<\/a>"}]},{"method":"addWMS","args":["https://geo.vliz.be/geoserver/Ecoregions/wms?cql_filter=ecoregion%20=%20'Azores%20Canaries%20Madeira'",null,null,{"styles":"","format":"image/png","transparent":true,"version":"1.1.1","info_format":"text/html","attribution":"<a href='https://marineregions.org/'>Marine Regions<\/a>","layers":"ecoregions","checkempty":false},null]},{"method":"addTiles","args":["https://tiles.emodnet-bathymetry.eu/osm/labels/inspire_quad/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false}]}]},"evals":[],"jsHooks":[]}
# View all the Extended Continental Shelf (ECS) boundary lines published during the first
# decade of the 21st century
mrp_view_ecs_boundaries(
  cql_filter = "doc_date > '2000-01-01' AND doc_date < '2009-12-31'"
)

{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG4326","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addTiles","args":["https://tiles.emodnet-bathymetry.eu/2020/baselayer/inspire_quad/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"<a href='https://emodnet.ec.europa.eu/en'>EMODnet<\/a>"}]},{"method":"addWMS","args":["https://geo.vliz.be/geoserver/MarineRegions/wms?cql_filter=doc_date%20%3E%20'2000-01-01'%20AND%20doc_date%20%3C%20'2009-12-31'",null,null,{"styles":"","format":"image/png","transparent":true,"version":"1.1.1","info_format":"text/html","attribution":"<a href='https://marineregions.org/'>Marine Regions<\/a>","layers":"ecs_boundaries","checkempty":false},null]},{"method":"addTiles","args":["https://tiles.emodnet-bathymetry.eu/osm/labels/inspire_quad/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false}]}]},"evals":[],"jsHooks":[]}
# Or as timestamp
mrp_view_eez_boundaries(
  cql_filter = "doc_date AFTER 2000-01-01T00:00:00Z AND doc_date BEFORE 2009-12-31T00:00:00Z"
)

{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG4326","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addTiles","args":["https://tiles.emodnet-bathymetry.eu/2020/baselayer/inspire_quad/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"<a href='https://emodnet.ec.europa.eu/en'>EMODnet<\/a>"}]},{"method":"addWMS","args":["https://geo.vliz.be/geoserver/MarineRegions/wms?cql_filter=doc_date%20AFTER%202000-01-01T00:00:00Z%20AND%20doc_date%20BEFORE%202009-12-31T00:00:00Z",null,null,{"styles":"","format":"image/png","transparent":true,"version":"1.1.1","info_format":"text/html","attribution":"<a href='https://marineregions.org/'>Marine Regions<\/a>","layers":"eez_boundaries","checkempty":false},null]},{"method":"addTiles","args":["https://tiles.emodnet-bathymetry.eu/osm/labels/inspire_quad/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false}]}]},"evals":[],"jsHooks":[]}# }
```
