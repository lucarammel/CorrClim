devtools::load_all('corrclim')
library(testthat)

test_that('API Enedis',{
  extract_weather_enedis = api_get_weather_enedis(start_date = lubridate::ymd("20220101"), end_date = lubridate::ymd("20220103")) 
  expect_true(is.data.table(extract_weather_enedis), info = "Object should be a data.table")
  
  expect_true(nrow(extract_weather_enedis) > threshold, info = "Object length should be greater than the 0")
})

test_that('API Holidays',{
  extract_holidays = api_get_holidays(lubridate::ymd("20210101"),
                                      lubridate::ymd("20220103"))
  
  expect_true(length(extract_holidays) > 0, info = "Object length should be greater than the 0")
})

test_that('API Meteo France', {
  result  = api_get_stations(departement = 1)
  expect_true(is.data.table(result), info = "Object should be a data.table")
  
  result = api_get_weather_meteo_france(c("01014002"), date_start = as.Date('2023-01-01') , 
                                        date_end = as.Date('2023-01-02'))
  
  expect_true(is.list(result), info = "Object should be a list")
})