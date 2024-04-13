rm(list=ls())
devtools::load_all('corrclim')
library(testthat)

dt = readRDS('tests/toy/timeseries.rds')

test_that("Test functionnalities",{
  ts = TimeseriesDT$new(dt)
  
  ts$remove_duplicated()$compute_instant()$add_suffix('y','_grad')$aggregate('day')$shift('y_grad', n = 2)$select('y_grad_shifted')
  
  granularity = ts$get_granularity()

  }
)
