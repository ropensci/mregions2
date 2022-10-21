# Check if cache dir exists and create if doesnt
mr_cache <- function(){

  path_cache <- file.path(Sys.getenv("MR_CACHE", unset = getwd()), "mr-cache/")
  # path_graph <- file.path(path_cache, "mr_graph.rdf")
  verbose <- getOption("verbose", default = TRUE)

  if(!dir.exists(path_cache)){
    dir.create(path_cache)

    if(verbose){
      cli::cli_alert_success(glue::glue("Cache dir created in: `{path_cache}`"))
    }

  }

  path_cache
}


# Creates a basic request with general settings
mr_request <- function(url, ...){
  req <- httr2::request(url) %>%
    httr2::req_user_agent("mregions2") %>%
    httr2::req_cache(mr_cache())

  # Add messages if verbose = TRUE
  verbose <- getOption("verbose", default = FALSE)
  if(verbose){
    req <- req %>%
      httr2::req_verbose(...) %>%
      httr2::req_cache(mr_cache(), debug = TRUE)
  }

  req
}

# Performs the request
mr_req_perform <- function(req, method = "GET"){
  resp <- req %>%
    httr2::req_perform(method)

  verbose <- getOption("verbose", default = FALSE)
  if(verbose){
    resp %>%
      httr2::resp_body_string() %>%
      cat()
  }

  resp
}


# Build base graph using known ontologies
mr_graph_base_build <- function(){

  # Cache control
  mr_graph_base_path = file.path(mr_cache(), "mr_graph_base.ttl")

  if(file.exists(mr_graph_base_path)){
    seconds_since_creation = as.numeric(difftime(Sys.time(), file.mtime(mr_graph_base_path), units = "secs"))
    max_age = 604800

    if(seconds_since_creation < max_age){
      out <- readLines(mr_graph_base_path) %>%
        paste0(collapse = "\n")

      verbose <- getOption("verbose", default = TRUE)
      if(verbose){
        cli::cli_alert_success(glue::glue("Cached base graph is fresh. Reading from `{mr_graph_base_path}`"))
      }

      # Premature end
      return(out)

    }else if(seconds_since_creation > max_age){
      file.remove(mr_graph_base_path)

    }

  }

  # If not cached, then perform

  # Get Turtle docs
  uri_turtle <- list(
    mr = "http://marineregions.org/ns/ontology",
    mrt = "http://marineregions.org/ns/placetypes",
    dcat = "https://www.w3.org/ns/dcat2",
    prov = "http://www.w3.org/ns/prov"
  )

  reqs_turtle <- lapply(uri_turtle, mr_request) %>%
    lapply(httr2::req_headers, accept = "text/turtle") %>%
    httr2::multi_req_perform(cancel_on_error = T) %>%
    lapply(httr2::resp_body_string) %>%
    unlist()

  # Get RDFXML docs
  uri_rdfxml<- list(
    skos = 'https://www.w3.org/2009/08/skos-reference/skos.rdf',
    gsp = 'http://www.opengis.net/ont/geosparql'
  )

  req_skos <- mr_request('https://www.w3.org/2009/08/skos-reference/skos.rdf') %>%
    httr2::req_headers(accept = 'application/xml+rdf') %>%
    mr_req_perform() %>%
    httr2::resp_body_string() %>%
    rdflib::rdf_parse("rdfxml") %>%
    rdflib::rdf_serialize(format = "turtle")

  req_gsp <- mr_request('http://www.opengis.net/ont/geosparql') %>%
    httr2::req_headers(accept = 'application/xml+rdf') %>%
    mr_req_perform() %>%
    httr2::resp_body_string() %>%
    rdflib::rdf_parse("rdfxml") %>%
    rdflib::rdf_serialize(format = "turtle")


  out <- reqs_turtle %>%
    c(req_skos, req_gsp) %>%
    paste0(collapse = "\n")

  # Cache
  file.create(mr_graph_base_path)
  writeLines(out, con = mr_graph_base_path)

  out
}
#
# options(verbose = TRUE)
#
# base_graph <- mr_graph_base_build()












