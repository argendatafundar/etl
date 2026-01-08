#############################################
# UNESCO Explore API v2.1 – R Wrapper
# Autor: listo para uso directo
#############################################


library(httr)
library(jsonlite)
library(dplyr)

# Base URL
.UNESCO_BASE_URL <- "https://data.unesco.org/api/explore/v2.1"


build_url <- function(endpoint, query_params = list()){
  url <- paste0(.UNESCO_BASE_URL, endpoint)
  url <- modify_url(url, query = query_params)
  return(url)
}


make_request <- function(url){
  response <- GET(url)
  if (status_code(response) == 200) {
    return(fromJSON(content(response, as = "text")))
  } else {
    stop("Error al obtener los datos: ", status_code(response))
  }
}

UNESCO_v21.get_catalog <- function(format = "json", limit = -1){
    endpoint = glue::glue("/catalog/exports/{format}")
    url = build_url(endpoint, query_params = list(limit = limit))
    response = make_request(url)
    return(response)
}


UNESCO_v21.get_dataset <- function(dataset_id, format = "json", limit = -1){
    endpoint = glue::glue("/catalog/datasets/{dataset_id}/exports/{format}")
    url = build_url(endpoint, query_params = list(limit = limit))
    response = make_request(url)
    return(response)
}

