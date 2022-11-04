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
    httr2::req_user_agent("mregions2")
  # %>%
  #   httr2::req_cache(mr_cache())

  # Add messages if verbose = TRUE
  verbose <- getOption("verbose", default = FALSE)
  if(verbose){
    req <- req %>%
      httr2::req_verbose(...) %>%
      httr2::req_cache(mr_cache(), debug = TRUE)
  }

  req
}


# Build base graph using known ontologies
mr_graph_base_build <- function(){

  # Cache control
  mr_graph_base_path = file.path(mr_cache(), "mr_graph_base.rdf")

  if(file.exists(mr_graph_base_path)){
    seconds_since_creation = as.numeric(difftime(Sys.time(), file.mtime(mr_graph_base_path), units = "secs"))
    max_age = 604800

    if(seconds_since_creation < max_age){
      graph <- rdflib::rdf_parse(mr_graph_base_path, format = "rdfxml")

      verbose <- getOption("verbose", default = TRUE)
      if(verbose){
        cli::cli_alert_success(glue::glue("Cached base graph is fresh. Reading from `{mr_graph_base_path}`"))
      }

      # Premature end
      return(graph)

    }else if(seconds_since_creation > max_age){
      file.remove(mr_graph_base_path)

    }

  }

  # If not cached, then perform

  # Define URIs
  namespaces <- list(
    mr = c(uri = "http://marineregions.org/ns/ontology#", accept = "text/turtle", format = "turtle"),
    mrt = c(uri = "http://marineregions.org/ns/placetypes#", accept ="text/turtle", format = "turtle"),
    dcat = c(uri = "https://www.w3.org/ns/dcat2#", accept ="text/turtle", format = "turtle"),
    prov = c(uri = "http://www.w3.org/ns/prov#", accept ="text/turtle", format = "turtle"),
    dc = c(uri = "http://purl.org/dc/terms/", accept ="text/turtle", format = "turtle"),
    rdfs = c(uri = 'http://www.w3.org/2000/01/rdf-schema#',  accept = "text/turtle", format = "turtle"),
    skos = c(uri = 'https://www.w3.org/2009/08/skos-reference/skos.rdf#', accept ="application/xml+rdf", format = "rdfxml"),
    gsp = c(uri = 'http://www.opengis.net/ont/geosparql#',  accept = "application/xml+rdf", format = "rdfxml")
    # xsd = c(uri = 'http://www.w3.org/2001/XMLSchema#',  accept = "application/xml", format = "rdfxml")
    # alternates: {"XMLSchema.dtd" 1 {type text/plain} {length 16075}}, {"XMLSchema.html" 1 {type application/xhtml+xml} {charset utf-8} {length 5808}},{"XMLSchema.xsd" 1 {type application/xml} {length 87677}}

  )

  # Initialize graph
  graph <- rdflib::rdf()

  # Fill graph
  for (namespace in namespaces){
    mr_request(namespace[1]) %>%
      httr2::req_headers(accept = namespace[2]) %>%
      httr2::req_perform() %>%
      httr2::resp_body_string() %>%
      rdflib::rdf_parse(rdf = graph, format = namespace[3]) %>%
      invisible()
  }

  # Cache graph
  namespace <- do.call(rbind, namespaces)[,1]
  rdflib::rdf_serialize(graph, doc = "./mr-cache/mr_graph_base.rdf", format = "rdf", namespace = namespace)

  # End
  graph
}





