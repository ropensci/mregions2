req_mr_user_agent <- function(.){
  httr2::req_user_agent(. , glue::glue("mregions2 {packageVersion('mregions')}"))
}
