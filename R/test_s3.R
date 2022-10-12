new_MRCollection <- function(x = list()){
  structure(x, class = "MRCollection")
}

new_MRGeoObject <- function(x = list()){
  structure(x, class = "MRGeoObject")
}


mr_get <- function(mrgid, ...){
  UseMethod("mr_get")
}


# Give mrgids, return the geoobject
# mr_get(3293)
mr_get.default <- function(mrgid, ...){

  # Request
  mr_get_n <- function(x){
    req <- httr2::request("http://marineregions.org") %>%
      httr2::req_url_path_append("mrgid") %>%
      httr2::req_url_path_append(x) %>%
      httr2::req_headers(accept = "application/ld+json") %>%
      req_mr_user_agent()

    resp <- req %>%
      httr2::req_perform() %>%
      httr2::resp_body_json()

    resp <- lapply(resp, new_MRGeoObject)
  }

  resp <- lapply(mrgid, mr_get_n) %>%
    new_MRCollection()

  resp

}

# Print nice info about the geoobject
print.MRCollection <- function(x, ...){

  # Assertions
  if(class(x)!="MRCollection") stop("The argument Object must be of class MRCollection")

  # Config
  mr_strtrim = function(x, width = getOption("mr_max_width", default = 80)){
    if(!is.null(x)){
      if(nchar(x) > width){
        x = strtrim(x, width) %>%
          paste0(" [...]")
      }
    }

    x
  }

  has_name = function(x, name){
    name %in% x
  }

  collapse_semicolon = function(x) paste0(x, collapse = ", ")

  par = list(
    `skos:prefLabel` = lapply(x, "[[", "skos:prefLabel") %>%
      lapply("[[", 1) %>%
      sapply("[[", "@value") %>%
      collapse_semicolon(),

    `mrt:PlaceType` = lapply(x, "[[", "@type") %>% sapply("[[", 2) %>% collapse_semicolon(),

    `@mrgid` = paste0("<", sapply(x, "[[", "@id"), ">", collapse = ", "),

    `dcat:centroid` = sapply(x, "[[", "dcat:centroid") %>% collapse_semicolon(),

    `dcat:bbox` = sapply(x, "[[", "dcat:bbox") %>% collapse_semicolon(),

    `mr:hasGeometry` = lapply(x, names) %>%
      sapply(has_name, "mr:hasGeometry") %>%
      collapse_semicolon()
    # ,

    # `prov:hadPrimarysource` = x[["prov:hadPrimarySource"]][["rdfs:label"]]
  )

  par = lapply(par, mr_strtrim)

  # Print
  glue::glue("Marine Regions collection of {length(x)} GeoObject(s)") %>% cat(sep = "\n")
  paste0("   `", names(par), "`= ", par) %>% cat(sep = "\n")

  invisible(x)
}




rm(print.MRCollection)

x <- mr_get(c(14, 15))
x <- mr_get(c(3293, 2581, 63295))
x







sapply(x, "[[", 1)



rm(print.MRGeoObject)
length(x)




lapply(x, names)


# mrgid
sapply(x, "[[", "@id")




# prefLabel
lapply(x, "[[", "skos:prefLabel") %>%
  lapply("[[", 1) %>%
  sapply("[[", "@value")

# PlaceType mrt:PlaceType
lapply(x, "[[", "@type") %>% sapply("[[", 2)



# altLabel
# lapply(x, "[[", "skos:altLabel") %>%

# centroid
sapply(x, "[[", "dcat:centroid")

# bbox
sapply(x, "[[", "dcat:bbox")

# has_geometry
has_name = function(x, name){
  name %in% x
}
lapply(x, names) %>%
  sapply(has_name, "mr:hasGeometry")

# has primary source
# lapply(x, "[[", "prov:hadPrimarySource")

# modified
sapply(x, "[[", "dc:modified")








str(x)



subset(x[[1]], names(x[[1]]))


mrgid <- lapply(x, subse)

paste0()







# S3
class(x)
x
as_tibble(x)

# R6
mr_get(14)$print()



cat(" [ reached getOption(\"width\") -- omitted", 10, "entries ]\n")

methods("summary")
methods(class = "Date")
.S3methods(class = "Date")
.S3PrimitiveGenerics



x[[1]]



## Core functionality

# Pretty print
# print.MRGeoObject

# Transform to data.frame / tibble / sf
# as.data.frame.MRGeoObject
# as.data.frame.MRCollection
# tibble::as_tibble.MRGeoObject
# tibble::as_tibble.MRCollection
# sf::st_as_sf.MRGeoObject
# sf::st_as_sf.MRCollection
# sf::st_as_sfc.MRGeoObject
# sf::st_as_sfc.MRCollection


## Helpers
# mr_view
# Opens a leaflet viewer that reads by default the most detailed geometry available:
# if there is geometry, read WMS link
# if no geometry but bbox, turn into sf and add to viewer
# if no bbox but centroid, turn into sf and add to viewer
# if none, raise error and ask to contact administrator











library(mregions2)
library(magrittr)

x <- mr_get(c(3293, 2581, 63295))




# Geometry set for 1 feature
# Geometry type: MULTIPOLYGON
# Dimension:     XY
# Bounding box:  xmin: 2.546263 ymin: 49.49607 xmax: 6.405904 ymax: 51.50408
# CRS:           EPSG:4326
# MULTIPOLYGON (((4.842145 51.45739, 4.836916 51....


t = list("caballo", "martillo")
names(t) <- c("animales", "herramientas")

cat(
  paste0(
    strtrim(
      paste0(names(t), ": ", t),
      15
    ),
    "..."
  )
, sep = "\n")

paste0("\n", "   ", names(x), ": ", x)






cat(t)







list(
  mrgid = 1,
  caballo = 2
)


pretty_print = function(x) cat(glue::glue('{names(x)}: `{x}` \n'))

for(i in 1:2){
  pretty_print(t[1])
}

lapply(t, pretty_print)

# A Marine Regions GeoObject of length 1:
#   mrgid: `http://marineregions.org/mrgid/14`
#   prefLabel: `French part of Bay of Biscay and the ...`
#   dcat:centroid





type = x[["63295"]][["@type"]][[2]]
mr:isPartOf = x[["63295"]][["mr:isPartOf"]][[1]][["skos:prefLabel"]]["@value"]

  ## Marine Regions GeoObject: 2 features
  ## prefLabel: 'Belgian part of the North Sea', ''
  ## Centroid:
  ## Bounding Box:
  ##
  ##

  cat( "Marine Region:\n")
  cat("  $name: ", private$..name, "\n", sep = "")
  cat("  $mrgid: ", private$..mrgid, "\n", sep = "")
  cat("  $latitude: ", private$..latitude, "\n", sep = "")
  cat("  $longitude: ", private$..longitude, "\n", sep = "")
  cat("  $placetype: ", private$..placetype, "\n", sep = "")
  cat("  $source: ", private$..source, "\n", sep = "")
  cat("  $status: ", private$..status, "\n", sep = "")
  cat("  $relations: A ", class(private$..relations)[2], " with ", nrow(private$..relations), " related marine regions.", "\n", sep = "")
  cat(message("              Tip: Explanations of relation types at: https://marineregions.org/ontology/documentation.html#objectproperties"))
  cat("  $add_geometry: ", private$..add_geometry, "\n", sep = "")
  cat("  $geometry: ", sep = "")
  private$get_geometry_info(private$..geometry)
  invisible(self)


  ## Geometry type: MULTIPOLYGON
  ## Dimension:     XY
  ## Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
  ## Geodetic CRS:  NAD27

  ## Reading layer `nc' from data source
  ##   `/tmp/RtmpRfSE0u/temp_libpath1418b6fc58031/sf/shape/nc.shp'
  ##   using driver `ESRI Shapefile'
  ## Simple feature collection with 100 features and 14 fields
  ## Geometry type: MULTIPOLYGON
  ## Dimension:     XY
  ## Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
  ## Geodetic CRS:  NAD27




  # cat(message("this is a geoobject"))
  x <- unlist(x)
  x <- subset(x, grepl("@context", names(x)))
  x <- new_mr_geo_object(x)

  print.simple.list(x)


library(magrittr)
x <- mr_get(3293)

x[["3293"]][["skos:prefLabel"]][[1]]$`@value`

#> # A tibble: 1 × 1
#>       x
#>   <dbl>
#> 1  123.

test <- mr_get(14)
test1 <- print(test)

test1 <- unlist(test)

print.simple.list(test)





