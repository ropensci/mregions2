library(R6)
library(mregions2)

# Structure:
# - Make all functionalities retrieved from the geoserver & gazetteer private (we can choose if users can alter them or not)
# - fetch all info about one record during the creation (initialisation)
# - Write function that users can easily add things to the object without having to know anything about OOP
# - make a wrapper function for the creaion of a new object (users don't have to write `new_obj <- mr_marine_region$new()`)
# - have default private elements for the user to explore (MRGID 3293)
# - outsource important variables like lat lon, source?, status into the private fields. make all 14 vars accessible via sth like `private$info_details`

# TODO: think about if add_geometry has to be a private element or if it can simply be public as well
# TODO: think about: how do users specify which marineregion they want while creating a new object? -> initialise
# TODO: create assertion when people want to override read-only elements, eg MRGID
# bpns$mrgid <- 2393
# error now: Error in (function ()  : unused argument (base::quote(2393))
#error goal: this is read-only. create a new marineregion with your MRGID
# TODO: print() should print the set parameters, not the default parameters
# TODO: think about if a name should be accepted as input since names are not unique.
## possible compromise: let name be used, but state in the documentation that always the first result from `mr_gaz_records_by_name()` will be used.
## So that it's advised to provide the mrgid
# TODO: document like https://roxygen2.r-lib.org/articles/rd.html#r6

marine_region <- R6Class(
  "MarineRegion",
  private = list(
    ..add_geometry = TRUE,
    ..mrgid = 3293,
    ..name = "Belgian Exclusive Economic Zone"
    # ,
    # ..information = mregions2::mr_gaz_records_by_name(private$..name) #if that does not work, define either a private or public method? sth like `get_info()`
  ),
  active = list(
    add_geometry = function(value){
      if(!missing(value)){
        private$..add_geometry <- value
      } else{ # `add_geometry` can be set by the user. can be easliy changed into read only. then it can only be set while creating the object
        # assertions
        private$..add_geometry
      }
    },
    mrgid = function(){private$..mrgid},
    name = function(){private$..name}
  ),
  public = list(
    initialize = function(add_geometry, mrgid, name){
      # assertions
      # todo: add assertions to check if class etc are correct

      if(!missing(add_geometry)){
        private$..add_geometry <- add_geometry
      }
      if(!missing(mrgid)){
        private$..mrgid <- mrgid
      }
      if(!missing(name)){
        private$..name <- name
      }
    },
    print = function(...){ #good style according to: https://adv-r.hadley.nz/r6.html
      # cat(mr_marine_region$classname, ":\n") # does not work for some reason
      cat( "Marine Region:\n")
      cat("  Name: ", private$..name, "\n", sep = "")
      cat("  MRGID: ", private$..mrgid, "\n", sep = "")
      cat("  add_geometry: ", private$..add_geometry, "\n", sep = "")
      invisible(self)
    }
  )
)

#' Get a marine region
#'
#' @description If only a name is given, the MRGID from the first record found by `mr_gaz_record_by_name(name)` will be fetched. Since names are not unique, prefer to input MRGIDs.
#' @param name
#' @param mrgid
#' @param add_geometry
#'
#' @return
#' @export
#'
#' @examples
#' german_part_north_sea <- mr_marine_region$new(name = "German Part of the North Sea")
#' high_seas_mrgid <- mr_gaz_record_by_name("High Seas", count = 1)$MRGID
#' high_seas <- mr_marine_region$new(high_seas_mrgid, add_geometry = FALSE)
mr_marine_region <- function(name = NA, mrgid = NA, add_geometry = TRUE){
  # assertions: check class of name and mrgid
  if(!missing(add_geometry)){
    checkmate::assert_logical(add_geometry)
  }
  if(is.na(name) & is.na(mrgid)){
    # warning("Either `name` or `MRGID` must be given.\nFind records by names with `?mr_gaz_records_by_names()` and by MRGIDs with `?mr_gaz_record()`.")
    message("Neither `name` nor `MRGID` were given. Therefore, the example `MarineRegion` is displayed.")
    res <- marine_region$new()
  } else {
    if((is.na(name) & !is.na(mrgid)) | (!is.na(name) & !is.na(mrgid))){
      # get name if no name was given or potentially correct name if name misspelled but given together with MRGID
      checkmate::assert_numeric(mrgid)
      name <- mregions2::mr_gaz_record(mrgid, add_geometry = FALSE)$preferredGazetteerName
    }
    if(is.na(mrgid) & !is.na(name)){
      # get MRGID if only name is given
      checkmate::assert_string(name)
      # if name is misspelled: make tryCatch error msg from mr_gaz_records_by_name() visible here
      mrgid <- mregions2::mr_gaz_records_by_name(name, count = 1)$MRGID
    }
    res <- marine_region$new(name = name, mrgid = mrgid, add_geometry = add_geometry)
    }
  return(res)
}


mr_marine_region(name = "Hotzenplotz")
mr_gaz_records_by_name(name = "Hotzenplotz")
mr_marine_region(name = "High Seas")
marine_region$new()

bpns <- mr_marine_region$new(name = "High Seas", add_geometry = FALSE)
class(bpns)
# View(bpns)
bpns$add_geometry <- FALSE
bpns$add_geometry
bpns$mrgid
# bpns$mrgid <- 2393
bpns
bpns$name

germany <- mr_marine_region$new(name = "German Part of the North Sea")
germany

default <- mr_marine_region$new()
default
