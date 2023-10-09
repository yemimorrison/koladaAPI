# In koladaAPI/R/koladaAPI.R
#' Gets multiple data from the API
#'
#' @param endpoint The API endpoint URL
#' @param data_api The type of data description
#' @param entities   A list of query parameters
#' 
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
    
    return(url)
    
    #response <- httr::GET(url)
    #httr::content(response, "parsed")
  } else {
    stop("Invalid number of entities. Provide 2 or 3 entities.")
  }
  
  
}


# Example usage for data_api = "data" with two entities
query_data <- get_query_data("http://api.kolada.se/v2", data_api = "data", entities = c("kpi", "N03932","municipality", "0580,1480,1280,0180,0380,1980"))

# Example usage for data_api = "oudata" with three entities
#query_data <- get_query_data("http://api.kolada.se/v2", data_api = "oudata", entities = c("kpi", "N15033", "ou", "V15E144001301,V15E144001101", "year", "2009,2007"))
#query_data

query_data_frame <- fromJSON(query_data)
query_data_frame <- as.data.frame(query_data_frame)

for (i in 1:nrow(query_data_frame)){
  sub_df <- query_data_frame[[5]][[i]]
  query_data_frame[i, 5] <- sub_df[3,4]
  }
  
description <- c("Count", "KPI", "MunicipalityID", "Year", "UnemploymentRate")
colnames(query_data_frame)<- description

query_data_frame <- query_data_frame %>% 
  mutate(Municipality = case_when(
    MunicipalityID == "0180" ~ "Stockholm",
    MunicipalityID == "1480" ~ "Goteborg",
    MunicipalityID == "0580" ~ "Linkoping",
    MunicipalityID == "0380" ~ "Uppsala",
    MunicipalityID == "1280" ~ "Malmo",
    MunicipalityID == "1980" ~ "Vasteras",
    TRUE ~ NA_character_
  ))



