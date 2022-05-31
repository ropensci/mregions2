# library(R6)
# library(mregions2)

# Structure:
# - Make all functionalities retrieved from the geoserver & gazetteer private (we can choose if users can alter them or not)
# - fetch all info about one record during the creation (initialisation)
# - Write function that users can easily add things to the object without having to know anything about OOP
# - make a wrapper function for the creaion of a new object (users don't have to write `new_obj <- mr_marine_region$new()`)
# - have default private elements for the user to explore (MRGID 3293)
# - outsource important variables like lat lon, source?, status into the private fields. make all 14 vars accessible via sth like `private$info_details`
# - give the possibility to add own private elements with a wrapper function?
# - get more info such as area when initialising the object (how to get the area? --> spatial operations)
# - Question: necessary/good idea to write the object with pipes? Now there is quite some nesting but idk if using pipes will destroy anything in R6
# - enable user to save more info in the object (i.e. wrapper function to create public elements)
# - eventually look up bathymetry data from emodnet / gebco

# TODO: create assertion when people want to override read-only elements, eg MRGID
# bpns$mrgid <- 2393
# error now: Error in (function ()  : unused argument (base::quote(2393))
#error goal: this is read-only. create a new marineregion with your MRGID
# TODO: document like https://roxygen2.r-lib.org/articles/rd.html#r6
# TODO: function to check if geometry is available? sth like `mr_check_geometry_availability()`
# TODO: add relationship function
# TODO: make a pro function to access all info
# TODO: what if system is in diff lang?
# TODO: relations: make easily accessible functions like <marine_region>$children/parents/neighbors.
#       Make message in `print()`: get more relations with <marine_region>$relations(direction, type)
#       Remove "all relations".
# TODO: change default values to NULL
# TODO: link functions directly

#' R6 Class Representing a Marine Region Object
#'
#' @description A ´MarineRegion` object corresponds to a record in the Marine Gazetteer, available at \url{https://marineregions.org}.
#' It contains relevant information such as the Name, the MRGID (Marine Regions Geographic IDentifier), the Placetype, the Source,...
marine_region <- R6::R6Class(
  "MarineRegion",
  private = list(

    #' @field add_geometry TRUE/FALSE statement if a `MarineRegion`'s geometry should be added or not.
    ..add_geometry = TRUE,
    #' @field mrgid Marine Regions Geographic IDentifier.
    ..mrgid = 3293,
    #' @field name `preferredGazetteerName` of the `MarineRegion`.
    ..name = "Belgian Exclusive Economic Zone",
    #' @field info Information about the `MarineRegion`.
    ..info = NULL,
    #' @field geometry Geometry of the `MarineRegion`.
    ..geometry = NA,
    #' @field source Source of the `MarineRegion`.
    ..source = NULL,
    #' @field longitude Longitude of the `MarineRegion`.
    ..longitude = NULL,
    #' @field latitude Latitude of the `MarineRegion`.
    ..latitude = NULL,
    #' @field status Status of the `MarineRegion`. `status == "standard"` should be preferred.
    ..status = NULL,
    #' @field placetype Placetype of the `MarineRegion`.
    ..placetype = NULL,
    #' @field relations Related Regions of the `MarineRegion`.
    ..relations = NULL,
    #' @field area Area in `km^2` of the `MarineRegion`.
    ..area = NULL,
    get_info = function(){
      private$..info <- mregions2::mr_gaz_record(private$..mrgid, add_geometry = FALSE)
      private$..relations <- mregions2::mr_gaz_relations_full_by_MRGID(private$..mrgid)
      private$..latitude <- private$..info$latitude
      private$..longitude <- private$..info$longitude
      private$..source <- private$..info$gazetteerSource
      private$..status <- private$..info$status
      private$..placetype <- private$..info$placeType
    },
    get_geometry = function(){
      private$..geometry <- mregions2::mr_gaz_geometry(private$..mrgid)
      if(is.list(private$..geometry)){
        private$..area <- sf::st_area(private$..geometry)
        attributes(private$..area)$units$numerator[1:2] <- "km"
        private$..area <- private$..area / 1000000 # transform m^2 to km^2
      }
    },
    get_geometry_info = function(geometry){
      if(is.list(geometry)){
        cat("class ", class(geometry)[1], "\n", sep = "")
        cat("  area: ")
        print(private$..area)
        cat(message("Tip: run `library(mapview); mapview(<marine_region>$geometry)` to plot/visualise the geometry."),"\n", sep = "")
      } else {
        if(is.null(geometry)){
          cat("NULL\n", sep = "")
          cat(message("  geometry not available at marineregions.org\n", sep = ""))
        } else{cat(geometry, "\n\n", sep = "")}
      }
    }
    ),
  active = list( # active elements make private elements accessible by the user
    add_geometry = function(){private$..add_geometry},
    mrgid = function(){private$..mrgid},
    name = function(){private$..name},
    info = function(){private$..info},
    geometry = function(){private$..geometry},
    latitude = function(){private$..latitude},
    longitude = function(){private$..longitude},
    source = function(){private$..source},
    status = function(){private$..status},
    placetype = function(){private$..placetype},
    relations = function(){private$..relations},
    area = function(){private$..area}
  ),
  public = list(
#' Inititalize `Marineregion` object
#' @description This method gets called whenever a new `MarineRegion` object is created. It created all necessary attributes and e.g. includes that the object is immediately printed.
#' @param add_geometry specify if the geometry is wanted or not
#' @param mrgid the Marine Regions Geographic IDentifier
#' @param name the name of the `MarineRegion` object
#'
#' @return an initialized `MarineRegion` object
    initialize = function(add_geometry, mrgid, name){
      if(!missing(mrgid)) {private$..mrgid <- mrgid}
      if(!missing(name)) {private$..name <- name}
      if(!missing(add_geometry)) {private$..add_geometry <- add_geometry}
      if(private$..add_geometry == TRUE) {private$get_geometry()}
      private$get_info()
      self$print()
      message("Tip: run `View(<marine_region>$info)` to get an overview on the marine region.\n")
    },
#' Print important attributes of the class `MarineRegion`
#'
#' @return important attributes of the class `MarineRegion`.
#' @export
#'
#' @examples
#' guadelupe <- mr_marine_region(name = "Guadelupe")
#' guadelupe
#' @description if an instance of `MarineRegion` is called, the `print` function will be run.
    print = function(){ #good style according to: https://adv-r.hadley.nz/r6.html
      cat( "Marine Region:\n")
      cat("  Name: ", private$..name, "\n", sep = "")
      cat("  MRGID: ", private$..mrgid, "\n", sep = "")
      cat("  Latitude: ", private$..latitude, "\n", sep = "")
      cat("  Longitude: ", private$..longitude, "\n", sep = "")
      cat("  Placetype: ", private$..placetype, "\n", sep = "")
      cat("  Source: ", private$..source, "\n", sep = "")
      cat("  Status: ", private$..status, "\n", sep = "")
      cat("  Relations: A ", class(private$..relations)[2], " with ", nrow(private$..relations), " related marine regions.", "\n", sep = "")
      cat("  add_geometry: ", private$..add_geometry, "\n", sep = "")
      cat("  geometry: ", sep = "")
      private$get_geometry_info(private$..geometry)
      invisible(self) #TODO: make tip about visualisation appear below `geometry: TRUE`
    }
  )
)

#' Get a marine region
#'
#' @description
#' If only a name is given, the MRGID from the first record found by `mr_gaz_record_by_name(name)` will be fetched. Since names are not unique, prefer to input MRGIDs.
#' @param name The `preferredGazetteerName` of a marine gazetteer record. Can be retrieved with `mr_gaz_records_by_name()`.
#' @param mrgid The `Marine Regions Geographical IDentifier`.
#' @param add_geometry Statement if geospatial data should be retrieved.
#'
#' @return An instance of the class `MarineRegion`.
#' @export
#'
#' @examples
#' german_part_north_sea <- mr_marine_region(name = "German Part of the North Sea")
#' high_seas_mrgid <- mregions2::mr_gaz_records_by_name("High Seas", count = 1)$MRGID
#' high_seas <- mr_marine_region(mrgid = high_seas_mrgid, add_geometry = FALSE)
mr_marine_region <- function(name = NA, mrgid = NA, add_geometry = TRUE){
  if(!missing(add_geometry)){
    checkmate::assert_logical(add_geometry)
  }
  if(is.na(name) & is.na(mrgid)){
    message("Neither `name` nor `MRGID` were given. Therefore, the example `MarineRegion` is displayed.\nFind records by names with `?mr_gaz_records_by_names()` and by MRGIDs with `?mr_gaz_record()`.")
    res <- marine_region$new()
  } else {
    if((is.na(name) & !is.na(mrgid)) | (!is.na(name) & !is.na(mrgid))){
      # get name if no name was given or get potentially correct name if name misspelled but given together with MRGID
      checkmate::assert_numeric(mrgid)
      name <- mregions2::mr_gaz_record(mrgid, add_geometry = FALSE)$preferredGazetteerName
    }
    if(is.na(mrgid) & !is.na(name)){
      # get MRGID if only name is given
      checkmate::assert_string(name)
      # TODO: if name is misspelled: make tryCatch error msg from mr_gaz_records_by_name() visible here
      message("Tip: Use `MRGID` to access elements unmistakeably. Find mrgids with `mr_gaz_records_by_name(<your_region_name>)`. For inputs with a `name`, the first result from `mr_gaz_records_by_name()` will be used as the marine region.")
      mrgid <- mregions2::mr_gaz_records_by_name(name, count = 1)$MRGID
    }
    res <- marine_region$new(name = name, mrgid = mrgid, add_geometry = add_geometry)
    }
  return(res)
}

#' Add elements to a marine region #DOES NOT WORK YET
#'
#' @param region Must be an object of the class `MarineRegion`
#' @param extension_name Must be of class character.
#' @param extension_value Can be a value or a function.
#'
#' @return Object that inherits of class `MarineRegion` that contains the new element specified.
#' @export
#'
#' @examples
#' mexico <- mr_marine_region(mrgid = 2224)
#' children <- function(mrgid) mregions2::mr_gaz_relations_by_MRGID(mrgid, direction = "lower", type = "all")
#' mr_marine_region_extend(region = mexico,  extension_name = "children", extension_value = children)
mr_marine_region_extend <- function(region, extension_name, extension_value) {

  marine_region_extension <- R6::R6Class(
    region$name,
    inherit = marine_region,
    public = list(
      extension_name = extension_value
    )
  )
  res <- marine_region_extension$new(name = region$name, mrgid = region$mrgid, add_geometry = region$add_geometry)
  # res <- marine_region$set(which = "public", name = extension_name, extension_value, overwrite = FALSE)
  return(res)
  # TODO: add something in print method, i.e. update method (if that's possible)
}
