rm(list=ls())
devtools::load_all('corrclim')
library(testthat)

timeseries = readRDS('tests/toy/timeseries.rds')
weather_observed = readRDS('tests/toy/weather_observed.rds')
weather_target = readRDS('tests/toy/weather_target.rds')

test_that("Model Additive and Multiplicative Operator",{

    operators = c(OperatorAdditive$new(), OperatorMultiplicative$new())

    for (operator in operators){
        clf = GAM$new(formula = y ~ s(temperature) + s(posan) + jour_semaine + jour_ferie + ponts)
    
        climatic_corrector = ClimaticCorrector$new(timeseries_model = clf, operator = operator)
        climatic_corrector$fit(timeseries, weather_observed)
        prediction = climatic_corrector$apply(timeseries, weather_observed, weather_target)
    }
})