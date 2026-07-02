# Get a data product

Get a data product

## Usage

``` r
mrp_get(
  layer,
  path = getOption("mregions2.download_path", tempdir()),
  cql_filter = NULL,
  filter = NULL,
  count = NULL
)
```

## Arguments

- layer:

  (character) Identifier of the data product. See
  [mrp_list](https://docs.ropensci.org/mregions2/reference/mrp_list.md)

- path:

  (character) Path to save the requests. Default is
  [`base::tempdir()`](https://rdrr.io/r/base/tempfile.html). See
  details.

- cql_filter:

  (character) Contextual Query Language (CQL) filter. See details.

- filter:

  (character) Standard OGC filter specification. See details.

- count:

  (numeric) Maximum number of features to be retrieved.

## Value

An sf object with the Marine Regions data product

## Details

This function uses [WFS
services](https://en.wikipedia.org/wiki/Web_Map_Service) to download the
Marine Regions layers as ESRI Shapefiles.

### Caching

By default, the layers are downloaded to a temporal directory
([`base::tempdir()`](https://rdrr.io/r/base/tempfile.html)). You can
provide a path in the `path` argument. But you can also set a path with
`# options("mregions2.download_path" = "my/path/")`.

Because it is possible to add filters, each request is identified with a
crc32 hash, provided with
[`digest::digest()`](https://eddelbuettel.github.io/digest/man/digest.html)
and attached to the file downloaded.

Once a layer is downloaded, it will be read from the cache during the
next two weeks. To avoid this, simply delete the layers in the cache
path.

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
[`mrp_view()`](https://docs.ropensci.org/mregions2/reference/mrp_view.md)
to visualize the data product in advance,
[`mrp_colnames()`](https://docs.ropensci.org/mregions2/reference/mrp_colnames.md)
and
[`mrp_col_unique()`](https://docs.ropensci.org/mregions2/reference/mrp_col_unique.md)
to get the name, data type and unique values of a the columns of a data
product, useful to query with the arguments `cql_filter` or `filter`

## Examples

``` r
# \donttest{
# Set cache path. Default is a temporal directory
options(mregions2.download_path = tempdir())

getOption("mregions2.download_path")
#> [1] "/var/folders/8j/sfr9qqcj73j4p6nhwcfpr0th0000gn/T//RtmpP4IbsV"
#> [1] "/tmp/RtmpARLgoE"

# See the list of all data products
mrp_list
#> # A tibble: 21 × 8
#>    title                   namespace layer license citation doi   imis  abstract
#>    <chr>                   <chr>     <chr> <chr>   <chr>    <chr> <chr> <chr>   
#>  1 Exclusive Economic Zon… MarineRe… eez   Creati… "Flande… http… http… "Versio…
#>  2 Maritime Boundaries (v… MarineRe… eez_… Creati… "Flande… http… http… "Versio…
#>  3 Territorial Seas (12 N… MarineRe… eez_… Creati… "Flande… http… http… "Versio…
#>  4 Contiguous Zones (24 N… MarineRe… eez_… Creati… "Flande… http… http… "Versio…
#>  5 Internal Waters (v3, w… MarineRe… eez_… Creati… "Flande… http… http… "Versio…
#>  6 Archipelagic Waters (v… MarineRe… eez_… Creati… "Flande… http… http… "Versio…
#>  7 High Seas (v1, world, … MarineRe… high… Creati… "Flande… http… http… "High S…
#>  8 Extended Continental S… MarineRe… ecs   Creati… "Flande… http… http… "This d…
#>  9 Extended Continental S… MarineRe… ecs_… Creati… "Flande… http… http… "This d…
#> 10 IHO Sea Areas (v3)      MarineRe… iho   Creati… "Flande… http… http… "World …
#> # ℹ 11 more rows

# We want the Exclusive Economic Zones of Portugal. Let's first visualize the product:
mrp_view("eez")

{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG4326","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addTiles","args":["https://tiles.emodnet-bathymetry.eu/2020/baselayer/inspire_quad/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"<a href='https://emodnet.ec.europa.eu/en'>EMODnet<\/a>"}]},{"method":"addWMS","args":["https://geo.vliz.be/geoserver/MarineRegions/wms?",null,null,{"styles":"","format":"image/png","transparent":true,"version":"1.1.1","info_format":"text/html","attribution":"<a href='https://marineregions.org/'>Marine Regions<\/a>","layers":"eez","checkempty":false},null]},{"method":"addTiles","args":["https://tiles.emodnet-bathymetry.eu/osm/labels/inspire_quad/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false}]}]},"evals":[],"jsHooks":[]}
# See all the columns on this data product
mrp_colnames("eez")
#> # A tibble: 31 × 3
#>    layer colname    type  
#>  * <chr> <chr>      <chr> 
#>  1 eez   mrgid      int   
#>  2 eez   geoname    string
#>  3 eez   mrgid_ter1 int   
#>  4 eez   pol_type   string
#>  5 eez   mrgid_sov1 int   
#>  6 eez   territory1 string
#>  7 eez   iso_ter1   string
#>  8 eez   sovereign1 string
#>  9 eez   mrgid_ter2 int   
#> 10 eez   mrgid_sov2 int   
#> # ℹ 21 more rows

# We should query on sovereign
# See all the possible values of sovereign1, sovereign2 and sovereign3
sov1 = mrp_col_unique("eez", "sovereign1")
sov2 = mrp_col_unique("eez", "sovereign2")
sov3 = mrp_col_unique("eez", "sovereign3")

# Is Portugal a value in the sovereign1, 2 and 3?
"Portugal" %in% sov1
#> [1] TRUE
#> [1] TRUE

"Portugal" %in% sov2
#> [1] FALSE
#> [1] FALSE

"Portugal" %in% sov3
#> [1] FALSE
#> [1] FALSE

# Portugal is only in sovereign1. Let's write a CQL filter to get only
# the EEZs of Portugal, or those where Portugal is a party of a dispute or a joint regime
portugal_eez <- mrp_get("eez", cql_filter = "sovereign1 = 'Portugal'")

# If you perform this request again, it will be read from the cache instead
portugal_eez <- mrp_get("eez", cql_filter = "sovereign1 = 'Portugal'")
#> Cache is fresh. Reading: /tmp/RtmpARLgoE/eez-1951c8b7/eez.shp
#> (Last Modified: 2023-04-24 17:45:16)

# You can also limit the number of features to be requested
mrp_get("eez", count = 5)
#> Simple feature collection with 5 features and 31 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -173.7747 ymin: -56.22782 xmax: -10.93248 ymax: -4.537529
#> Geodetic CRS:  WGS 84
#> # A tibble: 5 × 32
#>   mrgid geoname    mrgid_ter1 pol_type mrgid_sov1 territory1 iso_ter1 sovereign1
#>   <int> <chr>           <int> <chr>         <int> <chr>      <chr>    <chr>     
#> 1  8444 United St…       8670 200NM          2204 American … ASM      United St…
#> 2  8379 British E…       8620 200NM          2208 Ascension  SHN      United Ki…
#> 3  8446 New Zeala…       8672 200NM          2227 Cook Isla… COK      New Zeala…
#> 4  8389 Overlappi…       8623 Overlap…       2208 Falkland … FLK      United Ki…
#> 5  8440 French Ex…       8656 200NM            17 French Po… PYF      France    
#> # ℹ 24 more variables: mrgid_ter2 <int>, mrgid_sov2 <int>, territory2 <chr>,
#> #   iso_ter2 <chr>, sovereign2 <chr>, mrgid_ter3 <int>, mrgid_sov3 <int>,
#> #   territory3 <chr>, iso_ter3 <chr>, sovereign3 <chr>, x_1 <dbl>, y_1 <dbl>,
#> #   mrgid_eez <int>, area_km2 <int>, iso_sov1 <chr>, iso_sov2 <chr>,
#> #   iso_sov3 <chr>, un_sov1 <dbl>, un_sov2 <dbl>, un_sov3 <dbl>, un_ter1 <dbl>,
#> #   un_ter2 <dbl>, un_ter3 <dbl>, geometry <MULTIPOLYGON [°]>
# }
```
