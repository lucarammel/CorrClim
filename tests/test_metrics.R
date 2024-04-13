rm(list=ls())
devtools::load_all('corrclim')
library(testthat)

timeseries_cc = readRDS('tests/toy/timeseries_climate_corrected.rds')
timeseries = readRDS('tests/toy/timeseries.rds')
weather_observed = readRDS('tests/toy/weather_observed.rds')
weather_target = readRDS('tests/toy/weather_target.rds')

test_that('Tests Metrics',{
  
    weather_observed = TimeseriesDT$new(weather_observed)
    weather_target = TimeseriesDT$new(weather_target)
    weather = weather_observed$merge(weather_target, suffixes = c('_observed', '_target'), inplace = FALSE)
    
    metrics = Metrics$new()
    metrics$MAPE(timeseries, timeseries_cc)
    metrics$RMSE(timeseries, timeseries_cc)
    metrics$BIAS(timeseries, timeseries_cc)
    metrics$plot_bar_volume_correction(timeseries, timeseries_cc)
    metrics$plot_timeseries(timeseries, 
                            timeseries_cc,
                            ts_to_compare_with = weather, 
                            variables_to_compare = c('temperature_observed', 'temperature_target'), 
                            granularity = 'day', 
                            ylabel_to_compare = 'Temperature', 
                            legend_difference = 'Climate correction')
    metrics$plot_scatter_timeseries(y_ts = timeseries, 
                                    second_y_ts = timeseries_cc,
                                    x_ts = weather_observed,
                                    xlabel = 'Temperature (C)', 
                                    ylabel = 'Consumption(MW)', 
                                    title = 'Consumption vs temperature', 
                                    legend = c('Observed','Climate correction'))
})
