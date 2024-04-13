#' Process weather data from ENEDIS API
#'
#' @param start_date `(string)` starting date, format : yyyy-mm-dd
#' @param end_date `(string)` end_date, format : yyyy-mm-dd
#'
#' @importFrom glue glue
#'
#' @export
api_get_weather_enedis = function(start_date, end_date) {
  
  tryCatch({
    start_date = as.Date(start_date)
  }, error = function(e) {
    stop("Start date format is not supported. Please provide date with yyyy/mm/dd or yyyy-mm-dd")
  }, finally = {
    start_date
  })
  
  tryCatch({
    end_date = as.Date(end_date)
  }, error = function(e) {
    stop("End date format is not supported. Please provide date with yyyy/mm/dd or yyyy-mm-dd")
  }, finally = {
    end_date
  })
  message("Requesting Enedis API for weather data..")
  
  df_weather = api_request_weather_enedis() %>%
    api_process_weather_enedis(., start_date, end_date)
  
  return(df_weather)
}


#' Request ENEDIS API for weather data
#'
#' @importFrom httr GET content
#' @importFrom magrittr %>% set_names
#'
#' @return df_weather `(data.table)` temperature data from enedis api
#'
api_request_weather_enedis = function() {
  
  api_url = 'https://data.enedis.fr/api/explore/v2.1'
  proxy_url = 'http://vip-users.proxy.edf.fr:3128'
  dataset_weather_enedis = 'donnees-de-temperature-et-de-pseudo-rayonnement'
  
  request = GET(glue("{api_url}/catalog/datasets/{dataset_weather_enedis}/exports/csv?delimiter=%3B&list_separator=%2C&quote_all=false&with_bom=true"))
  if (request$status_code == 200) {
    df_request = request %>%
      content(show_col_types = FALSE)
    columns = df_request %>%
      colnames %>%
      read.csv(text = ., sep = ";") %>%
      names
    df_weather = read.csv(text = df_request[[1]], sep = ";", dec = ".", header = FALSE) %>%
      set_names(columns)
    return(df_weather)
  } else {
    return()
  }
}


#' Process weather data from ENEDIS API
#'
#' @param df_weather `(data.table)` Data obtained after api_request_weather_enedis
#' @param start_date `(string)` starting date, format : yyyy/mm/dd
#' @param end_date `(string)` end_date, format : yyyy/mm/dd
#'
#' @importFrom glue glue
#' @importFrom data.table as.data.table
#' @importFrom dplyr mutate select filter arrange rename
#' @importFrom lubridate with_tz ymd_hms
#' @return None
#'
api_process_weather_enedis = function(df_weather, start_date, end_date) {
  
  timezone = 'UTC'
  
  dt = df_weather %>%
    as.data.table %>%
    .[(horodate >= start_date & horodate < end_date + 1)] %>% 
    .[, c("time", "temperature_observed", "temperature_normal") := .(
      horodate,
      temperature_realisee_lissee_degc,
      temperature_normale_lissee_degc
    )] %>% 
    .[, .(time, temperature_observed, temperature_normal)] %>% 
    .[order(time)]
  
  return(dt)
}


#' Get holidays from the different years
#'
#' @param start_date `(string)` starting date, format : yyyy/mm/dd
#' @param end_date `(string)` end_date, format : yyyy/mm/dd
#' @param request_country `(string)`  country code of the country requested. Example : 'FR'
#'
#' @import data.table
#' @importFrom magrittr %>% %<>%
#' @importFrom glue glue
#'
#' @return holidays `(vector)`
#'
#' @export
api_get_holidays = function(start_date, end_date, request_country = 'FR') {
  
  years = year(start_date):year(end_date)
  holidays = list()
  
  for (y in years) {
    response = api_request_holidays(request_year = y, request_country = toupper(request_country))
    
    if (!is.null(response)) {
      holidays[[as.character(y)]] = response %>%
        .$date %>%
        as.Date
    } else {
      warning("Warning : holidays for year {y} not downloaded correctly")
    }
  }
  if (length(holidays) != 0) {
    holidays %<>%
      as.data.table %>%
      melt.data.table %>%
      rename(year = "variable", holiday = "value") %>%
      select(-year) %>%
      .$holiday
  }
  
  return(holidays)
}


#' Request Nager API for holidays data
#'
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom magrittr %>%
#'
#' @return holidays (dataframe) for a year and a country
#'
#' @export
api_request_holidays = function(request_year, request_country) {
  
  response = GET(glue("https://date.nager.at/api/v3/PublicHolidays/{request_year}/{request_country}"))
  
  if (response$status_code == 200) {
    holidays = fromJSON(content(response, "text"))
    return(holidays)
  } else {
    return()
  }
}

#' Get Stations list from API Meteo France
#'
#' @importFrom httr GET content add_headers
#' @importFrom glue glue
#' @importFrom data.table rbindlist
#' 
#' @export
api_get_stations = function(departement){
  
  token = api_get_token_meteo_france()
  
  url = glue('https://public-api.meteofrance.fr/public/DPClim/v1/liste-stations/horaire?id-departement={departement}')
  
  headers = c('Authorization' = glue('Bearer {token}'))
  
  response = GET(url, add_headers(.headers = headers))
  
  status = response$status_code
  
  if (status == 200) {
    dt = rbindlist(content(response))
    
    return(dt)
  }
  else{
    stop(glue("Impossible to request stations list for Meteo France API. Error :{status}"))
  }
}

#' Get weather timeseries from api Meteo France
#'
#' @importFrom httr GET content add_headers
#' @importFrom glue glue
#' @importFrom data.table rbindlist
#' 
#' @export
api_get_weather_meteo_france = function(stations_id, date_start, date_end){
  
  date_start = as.POSIXct(date_start, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
  date_start = URLencode(format(date_start,"%Y-%m-%dT%H:%M:%SZ" ), reserved = TRUE)
  
  date_end = as.POSIXct(date_end, tz = "UTC",  format = "%Y-%m-%dT%H:%M:%SZ")
  date_end = URLencode(format(date_end, "%Y-%m-%dT%H:%M:%SZ"), reserved = TRUE)
  
  outputs = list()
  
  token = api_get_token_meteo_france()
  
  for (station_id in stations_id){
    url = glue("https://public-api.meteofrance.fr/public/DPClim/v1/commande-station/horaire?id-station={station_id}&date-deb-periode={date_start}&date-fin-periode={date_end}")
    headers = c('Authorization' = glue('Bearer {token}'))
    response = GET(url, add_headers(.headers = headers))
    
    status = response$status_code
    
    if (status == 202) {
      id_order = rbindlist(content(response))$return
    }
    else{
      stop(glue("Impossible to make a command for a csv download for Meteo France API. Error :{status}"))
    }
    
    url = glue('https://public-api.meteofrance.fr/public/DPClim/v1/commande/fichier?id-cmde={id_order}')
    response = GET(url, add_headers(.headers = headers))
    
    status = response$status_code
    
    if (status == 201) {
      df_request = response %>%
        content
      columns = df_request %>%
        colnames %>%
        read.csv(text = ., sep = ";") %>%
        names
      df_weather = read.csv(text = df_request[[1]], sep = ";", dec = ",", header = FALSE) %>%
        setNames(columns) %>% as.data.table
      
      outputs[[station_id]] = df_weather
    }
    else{
      stop(glue("Impossible to get the command id {id_order} stations list for Meteo France API. Error :{status}"))
    }
  }
  
  return(outputs)
}

#' Get Token for API Meteo France
#'
#' @importFrom httr POST content add_headers
#' @importFrom glue glue
#' 
#' @export
api_get_token_meteo_france = function(){
  
  url = "https://portail-api.meteofrance.fr/token"
  
  headers = c("Authorization" = "Basic Y0ROWTVNUDAxVXJLdEwyTnk2UWlZMnVvTkg0YTpZeGNnaVNnUDVyalJwVzlEc1dlZ1Z5R2ZuOW9h",  "Content-Type" = "application/json")
  body = list(grant_type = "client_credentials")
  
  response = POST(url, add_headers(.headers = headers), body = body, encode = "json")
  
  result = content(response)
  
  status = response$status_code
  token = result$access_token
  
  if (status == 200) {
    return(token)
  }
  else{
    stop(glue("Impossible to request token for Meteo France API. Error :{status}"))
  }
}





