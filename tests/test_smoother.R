rm(list=ls())
devtools::load_all('corrclim')
library(testthat)

# Loading data

timeseries = readRDS('tests/toy/timeseries.rds')
weather_observed = readRDS('tests/toy/weather_observed.rds')
weather_target = readRDS('tests/toy/weather_target.rds')

test_that("Test Smoother Exponential",{
  smoother = ExponentialSmoother$new(alpha = 0.02, granularity = 'step')
  
  smoother$fit(weather_observed)
  result = smoother$smooth(weather_observed)
  
})

test_that("Test Smoother GridSearch",{
  grid = list(alpha = seq(0.001, 1, 0.1))
  smoother = GridSearchSmoother$new(grid = grid, smoother = ExponentialSmoother)
  smoother$fit(weather_observed, timeseries)
  result = smoother$smooth(weather_observed)
  
})

test_that("Test Smoother Exponential Bayesian",{
  bounds = list(alpha = c(0.001, 1))
  smoother = BayesianSmoother$new(bounds = bounds, smoother = ExponentialSmoother)
  smoother$fit(weather_observed, timeseries)
  result = smoother$smooth(weather_observed)
  
})


test_that("Test MultiSmoother",{
  weather_observed[, temperature_test := temperature]

  smoother_grid = ExponentialSmoother$new(alpha = 0.02)
  smoother_bayes = ExponentialSmoother$new(alpha = 0.04)

  smoother = MultiSmoother$new(smoothers = c(smoother_grid, smoother_bayes), variables = c('temperature', 'temperature_test'))

  smoother$fit(weather_observed, timeseries)
  result = smoother$smooth(weather_observed)
  
})


test_that("Test Apply Smoother GAM",{
  smoother_exp = ExponentialSmoother$new(alpha = 0.7)
  
  clf = GAM$new(formula = y ~ s(temperature) + s(posan) + jour_semaine + jour_ferie + ponts, smoothers = smoother_exp)
  
  climatic_corrector = ClimaticCorrector$new(timeseries_model = clf)
  climatic_corrector$fit(timeseries, weather_observed, weather_target)
  prediction = climatic_corrector$apply(timeseries, weather_observed, weather_target)
  
})

test_that("Test Apply Smoother GAM, Dummy",{
  dummy_smoother = DummySmoother$new()
  
  clf = GAM$new(formula = y ~ s(temperature) + s(posan) + jour_semaine + jour_ferie + ponts, smoothers = dummy_smoother)
  
  climatic_corrector = ClimaticCorrector$new(timeseries_model = clf)
  climatic_corrector$fit(timeseries, weather_observed, weather_target)
  prediction = climatic_corrector$apply(timeseries, weather_observed, weather_target)
  
})

test_that("Test Apply Smoother GAM, Dummy and Exp",{
  
  smoother_exp = ExponentialSmoother$new(alpha = 0.5)
  dummy_smoother = DummySmoother$new()
  
  weather_observed[, temperature_test := temperature]
  weather_target[, temperature_test := temperature]
  
  clf = GAM$new(formula = y ~ s(temperature) + s(posan) + jour_semaine + jour_ferie + ponts, smoothers = MultiSmoother$new(c(dummy_smoother, smoother_exp), variables = c('temperature', 'temperature_test')))
  
  climatic_corrector = ClimaticCorrector$new(timeseries_model = clf)
  climatic_corrector$fit(timeseries, weather_observed)
  prediction = climatic_corrector$apply(timeseries, weather_observed, weather_target)
  
})
