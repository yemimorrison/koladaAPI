# In koladaAPI/R/getquerydata.R
#' Gets multiple data from the API
#'
#' @param endpoint The API endpoint URL
#' @param entity   A list of query parameters
#' @import dplyr
#' 
#' @return         Parsed API response
#'
#' @export

getquerydata <- function(endpoint, entity, municipality, year) {
  # Check if the endpoint matches the required format
  if (endpoint != "http://api.kolada.se/v2/data/kpi/N03932") {
    stop("Invalid endpoint. The endpoint must be 'http://api.kolada.se/v2/data/kpi/N03932'.")
  }
  
  # Check if entity is "municipality"
  if (entity != "municipality") {
    stop("Invalid entity. The entity must be 'municipality'.")
  }
  
  # Check if municipality is in the list of valid municipalities
  if (all(municipality %in% municipalities$Municipality)) {
    
    # Replace municipality values with corresponding IDs
    municipality <- municipalities$ID[match(municipality, municipalities$Municipality)]
    
  } else {  
    
    stop("Invalid municipality. The municipality provided is not in the list of valid municipalities.")
  }
  
  # Check if year is an integer and within the valid year range
  Year_Range <- 2010:2022
  if (!is.numeric(year) || any(year %% 1 != 0) || !all(year %in% Year_Range)) {
    stop("Invalid year. Year must be a whole number within the range 2010-2022.")
  }
  
  # Construct the full API endpoint URL
  api_url <- paste0(
    endpoint, "/", entity, "/", paste(municipality, collapse = ","), "/year/", paste(year, collapse = ",")
  )
  
  query_data_frame <- fromJSON(api_url)
  query_data_frame <- as.data.frame(query_data_frame)
  
  for (i in 1:nrow(query_data_frame)){
    sub_df <- query_data_frame[[5]][[i]]
    query_data_frame[i, 5] <- sub_df[3,4]
  }
  
  description <- c("Count", "KPI", "MunicipalityID", "Year", "UnemploymentRate")
  colnames(query_data_frame)<- description
  
  query_data_frame <- query_data_frame %>% 
    mutate(Municipality = municipalities$Municipality[match(MunicipalityID, municipalities$ID)])
  
  query_data_frame$UnemploymentRate <- as.numeric(as.character(unlist(query_data_frame$UnemploymentRate)))
  
  
  return(query_data_frame)
}


#querydata <- getquerydata("http://api.kolada.se/v2/data/kpi/N03932", entity = "municipality", municipality = c("Vallentuna","Jarfalla", "Stockholm", "Linkoping") , year = c(2010:2015))
#querydata

