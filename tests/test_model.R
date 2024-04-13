rm(list=ls())
devtools::load_all('corrclim')
library(testthat)

# Loading data

timeseries = readRDS('tests/toy/timeseries.rds')
weather_observed = readRDS('tests/toy/weather_observed.rds')
weather_target = readRDS('tests/toy/weather_target.rds')

test_that("Model GAM basic",{
  clf = GAM$new(formula = y ~ s(temperature) + s(posan) + jour_semaine + jour_ferie + ponts, by_instant = F)
  
  climatic_corrector = ClimaticCorrector$new(timeseries_model = clf)
  climatic_corrector$fit(timeseries, weather_observed)
  prediction = climatic_corrector$apply(timeseries, weather_observed, weather_target)
  
  expect_true(is.data.table(prediction$get_timeseries()))
  
})


test_that("Model GAM Std basic",{
  weather_observed[, year := year(time)]
  
  operator = Operator2Moments$new()
  
  timeseries_model = GAM$new(formula = y ~ s(temperature) + s(posan) + jour_semaine + jour_ferie + ponts)
  timeseries_std_model = GamStd$new(conditional_expectation_model = timeseries_model, formula = y ~ s(temperature) + s(posan) + jour_semaine + jour_ferie + ponts)
  
  climatic_corrector = ClimaticCorrector$new(timeseries_model = timeseries_model, timeseries_std_model = timeseries_std_model, operator = operator)
  climatic_corrector$fit(timeseries, weather_observed, fold_varname = 'year')
  
  prediction = climatic_corrector$apply(timeseries, weather_observed, weather_target)
  
  expect_true(is.data.table(prediction$get_timeseries()))
  
})

test_that("Model Prophet basic",{
  clf = Prophet$new(formula = y ~ temperature)
  
  climatic_corrector = ClimaticCorrector$new(timeseries_model = clf)
  climatic_corrector$fit(timeseries, weather_observed)
  prediction = climatic_corrector$apply(timeseries, weather_observed, weather_target)
  
  expect_true(is.data.table(prediction$get_timeseries()))
})

test_that("Model GradDelta basic",{
  clf = GradDelta$new(formula =y ~ temperature)
  
  climatic_corrector = ClimaticCorrector$new(timeseries_model = clf)
  climatic_corrector$fit(timeseries, weather_observed)
  prediction = climatic_corrector$apply(timeseries, weather_observed, weather_target)
  
  expect_true(is.data.table(prediction$get_timeseries()))
  
})

test_that("Model CopyCat basic",{
  clf = CopyCat$new(formula =y ~ temperature)
  
  climatic_corrector = ClimaticCorrector$new(timeseries_model = clf, operator = OperatorTarget$new())
  climatic_corrector$fit(timeseries, weather_observed)
  prediction = climatic_corrector$apply(timeseries, weather_observed, weather_target)
  
  expect_true(is.data.table(prediction$get_timeseries()))
  
})



