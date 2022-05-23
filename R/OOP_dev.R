library(R6)
library(mregions2)

# Structure:
# - Make all functionalities retrieved from the geoserver & gazetteer private (we can choose if users can alter them or not)
# - fetch all info about one record during the creation (initialisation)
# - Write function that users can easily add things to the object without having to know anything about OOP
# - make a wrapper function for the creaion of a new object (users don't have to write `new_obj <- mr_marine_region$new()`)
# - have default private elements for the user to explore (MRGID 3293)

# TODO: think about if geometry has to be a private element or if it can simply be public as well
# TODO: think about: how do users specify which marineregion they want while creating a new object? -> initialise
# TODO: create assertion when people want to override read-only elements, eg MRGID
# bpns$mrgid <- 2393
# error now: Error in (function ()  : unused argument (base::quote(2393))
#error goal: this is read-only. create a new marineregion with your MRGID
# TODO: make the tables (? what did I want there?)


mr_marine_region <- R6Class(
  "MarineRegion",
  private = list(
    ..geometry = TRUE,
    ..mrgid = 3293,
    ..name = "Belgian Exclusive Economic Zone"
    # ,
    # ..information = mregions2::mr_gaz_records_by_name(private$..name)
  ),
  active = list(
    geometry = function(value){
      if(!missing(value)){
        private$..geometry <- value
      } else{ # `geometry` can be set by the user. can be easliy changed into read only. then it can only be set while creating the object
        # assertions
        private$..geometry
      }
    },
    mrgid = function(){private$..mrgid},
    name = function(){private$..name}
  ),
  public = list(
    initialize = function(geometry, mrgid, name){
      # make assertion that either name or mrgid has to be specified
      # add assertions to check if class etc are correct
      if(!missing(geometry)){
        private$..geometry <- geometry
      }
      if(!missing(mrgid)){
        private$..mrgid <- mrgid
        invisible(mrgid) # return values which can be assigned, but which do not print when they are not assigned
      } else{
        #fetch mrgid from name
        # private$..mrgid <- mregions2::mr_gaz_records_by_name(name)$MRGID
        else_stmnt <- TRUE
      }
      if(!missing(name)){
        private$..name <- name
      } else{
        # get name from mrgid
        todo_mr_gaz_records_by_mrgid <- TRUE #little temporary easter egg reminding one of the task to create the function
      }
    },
    print = function(...){ #good style according to: https://adv-r.hadley.nz/r6.html
      cat(mr_marine_region$classname, ":\n")
      cat("  Name: ", private$..name, "\n", sep = "")
      cat("  MRGID: ", private$..mrgid, "\n", sep = "")
      cat("  Geometry: ", private$..geometry, "\n", sep = "")
      invisible(self)
    }
  )
)

bpns <- mr_marine_region$new(mrgid = 2350)
class(bpns)
# View(bpns)
bpns$geometry <- FALSE
bpns$geometry
bpns$mrgid
# bpns$mrgid <- 2393
bpns
bpns$name
bpns$private_fields$..mrgid


default <- mr_marine_region$new()
default
