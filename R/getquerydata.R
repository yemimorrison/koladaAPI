# In koladaAPI/R/koladaAPI.R
#' Gets multiple data from the API
#'
#' @param endpoint The API endpoint URL
#' @param data_api The type of data description
#' @param entities   A list of query parameters
#' @return         Parsed API response
#'
#' @export

get_query_data <- function(endpoint, data_api, entities) {
  # Check if the number of entities is valid (2 or 3)
  if (length(entities) %in% c(4, 6)) {
    # Create a list of query parameters based on the data_api type
    params <- list(
      continent = entities[1],
      year = entities[length(entities)]
    )
    
    # Check if there are two entities or three entities
    if (length(entities) == 6) {
      params[[entities[2]]] <- entities[3]
    }
    
    # Construct the full API endpoint URL
    url <- paste0(endpoint, "/", data_api, "/", paste(entities, collapse = "/"))
    
    #return(url)
    
    response <- httr::GET(url)
    httr::content(response, "parsed")
  } else {
    stop("Invalid number of entities. Provide 2 or 3 entities.")
  }
  
  
}

# Example usage for data_api = "data" with two entities
query_data <- get_query_data("http://api.kolada.se/v2", data_api = "data", entities = c("municipality", "1860", "year", "2009"))
query_data

# Example usage for data_api = "oudata" with three entities
query_data <- get_query_data("http://api.kolada.se/v2", data_api = "oudata", entities = c("kpi", "N15033,N15030", "ou", "V15E144001301,V15E144001101", "year", "2009,2008,2007"))
query_data

