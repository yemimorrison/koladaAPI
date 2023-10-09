# In koladaAPI/R/koladaAPI.R
#' Gets single data from the API
#'
#' @param endpoint The API endpoint URL
#' @param params   A list of query parameters
#' @return         Parsed API response
#'
#' @export

get_api_data <- function(endpoint, entity, params = list()) {
  
  
  url <- paste0(endpoint, "/", entity, "?", paste0(names(params), "=", params, collapse = "&"))
  
  return(url)
  
  #response <- httr::GET(url)
  #httr::content(response, "parsed")
  
}

api_data <- get_api_data("http://api.kolada.se/v2", entity = "municipality", params = list())
api_data_df <- fromJSON(api_data)
api_data_df <- as.data.frame(api_data_df)

kpi_data <- get_api_data("http://api.kolada.se/v2", entity = "kpi", params = list())
kpi_data_df <- fromJSON(kpi_data)
kpi_data_df <- as.data.frame(kpi_data_df)
