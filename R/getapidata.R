# In koladaAPI/R/getapidata.R
#' Gets single data from the API
#'
#' @param endpoint The API endpoint URL
#' @param entity   A required API variable from kolada
#' @param params   A list of query parameters
#' @return         Parsed API response
#' @import jsonlite
#' @import curl
#' 
#' @export

getapidata <- function(endpoint, entity, params = list()) {
  # Check if the endpoint matches the required format
  if (endpoint != "http://api.kolada.se/v2") {
    stop("Invalid endpoint. The endpoint must be 'http://api.kolada.se/v2'.")
  }
  
  #Check if the entity matches the required variables
  entities = c("kpi", "kpi_groups", "municipality", "municipality_groups","ou")
  
  if(!(entity %in% entities)){
    stop("Invalid entity. Entity must be one of the following:
        kpi,
        kpi_groups,
        municipality,
        municipality_groups,
        ou")
  }
  
  #Include 'per_page' parameter in the URL only if provided
  if(!is.null(params$per_page)){
    per_page <- as.numeric(params$per_page)
    if (per_page > 5000){
      stop("API Limit exceeded. Maximum number of results is 5000.")
    }
  apiurl <- paste0(endpoint, "/", entity, "?per_page=", per_page, "&", paste0(names(params)[-which(names(params) == "per_page")], "=", params[-which(names(params) == "per_page")], collapse = "&"))
  } else {
  apiurl <- paste0(endpoint, "/", entity, "?", paste0(names(params), "=", params, collapse = "&"))
  }
  
  api_data_df <- fromJSON(apiurl)
  api_data_df <- as.data.frame(api_data_df)
  return(api_data_df)
}

#result <- getapidata("http://api.kolada.se/v2", entity = "ou", params = list(title="skola", per_page = 5000))




