#' @title Prophet Model
#'
#' @docType class
#' @description
#'   This class represents a model (Prophet) for time series forecasting using the Prophet package.
#'
#' @field formula `(formula)` the formula object representing the fit wanted (e.g y ~ temperature)
#' @field yearly_seasonality Boolean, default TRUE. Indicates whether to include yearly seasonality.
#' @field weekly_seasonality Boolean, default TRUE. Indicates whether to include weekly seasonality.
#' @field daily_seasonality Boolean, default TRUE. Indicates whether to include daily seasonality.
#' @field interval_width Numeric, default 0. Indicates the uncertainty interval width.
#' 
#' @importFrom R6 R6Class
#' @importFrom magrittr %>% %<>% set_names
#' @import data.table
#' @import prophet
#' @export 
Prophet = R6Class("Prophet", 
                  inherit = TimeseriesModel,
                  public = list(
                    yearly_seasonality  = TRUE, 
                    weekly_seasonality = TRUE, 
                    daily_seasonality = TRUE, 
                    interval_width = 0, 
                    initialize = function(formula = y ~  temperature, ...){
                      
                      super$initialize(formula = formula, by_instant = FALSE, granularity = NULL, ...)
                      
                      self$model = prophet(yearly.seasonality = self$yearly_seasonality, 
                                           weekly.seasonality = self$weekly_seasonality, 
                                           daily.seasonality = self$daily_seasonality,
                                           interval.width = self$interval_width) 
                      
                      for (variable in self$formula$get_explanatory_variables()){
                        add_regressor(self$model, name = variable)
                      }
                    }, 
                    print = function(){
                      
                    }, 
                    #' @description 
                    #'  Fit function of the model itself
                    #' 
                    #' @param model `(Any)`
                    #' @param X `(data.table | TimeseriesDT)` Timeseries data to fit. Should contains all variables in the formula
                    fit_fun = function(model, X){
                      
                      X = TimeseriesDT$new(X)$get_timeseries()
                      setnames(X, 'time', 'ds')
                      return(fit.prophet(model, X))
                    },
                    #' @description 
                    #'  Predict function of the model itself
                    #' 
                    #' @param model `(Any)` The model trained
                    #' @param X `(data.table | TimeseriesDT)` Timeseries data to predict. 
                    #' @return `(vector)` The output of the prediction of the model on X. 
                    predict_fun = function(model, X){
                      
                      X = TimeseriesDT$new(X)$get_timeseries()
                      setnames(X, 'time', 'ds')
                      return(predict(model, X))
                    }
                  )
)

