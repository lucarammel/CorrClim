# Version 1.0 

* Build the global architecture with ClimaticCorrector, TimeseriesModel, TimeseriesDT, Formula, Smoother, Metrics objects from R6 Class.
* Includes API requests data for ENEDIS Open data and METEO FRANCE  
* Includes TimeseriesModel such as : GAM (by instant or not), GradDelta (by instant or not), Prophet, CopyCat
* Includes Smoother such as ExponentialSmoother, and optimized such as BayesianOptimisation or GridSearch. 
* Includes metrics model agnostic : ready to use plots on timeseries, and other metrics (rmse, bias, mape)