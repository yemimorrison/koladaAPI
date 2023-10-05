# In koladaAPI/R/koladaAPI.R
#' Gets data from the API
#'
#' @param endpoint The API endpoint URL
#' @param params   A list of query parameters
#' @return         Parsed API response
#'
#' @export

get_api_data <- function(endpoint, entity, param) {
  
  params <- list(title = param)
  
  url <- paste0(endpoint, "/", entity, "?", paste0(names(params), "=", params, collapse = "&"))
  response <- httr::GET(url)
  httr::content(response, "parsed")
}

api_data <- get_api_data("http://api.kolada.se/v2/", entity = "municipality", param = "lund")
api_data
