
#' @title GAM (Generative Additive Model)
#'
#' @docType class
#' @description
#'   The Generative Additive Model (GAM) for climate correction.
#' 
#' @field formula `(formula)` the formula object representing the fit wanted (e.g y ~ temperature)
#' @field by_instant `(boolean)`. If you want to perform a fit by instant of specific granularity
#' @field granularity `(Optional[string])` `{'day', 'month'}`. The granularity used to compute instant.
#'
#' @importFrom R6 R6Class
#' @importFrom R39Toolbox GeneralizedAdditiveModel
#' @import data.table
#' @export
GAM = R6Class("GAM", 
              inherit = TimeseriesModel,
              public = list(
                initialize = function(formula = y ~ s(temperature) + s(posan) + jour_semaine + jour_ferie + ponts, by_instant=TRUE, granularity = 'day', ...){
                  
                  super$initialize(formula = formula, by_instant = by_instant, granularity = granularity, ...)
                  
                  if (by_instant){
                    self$model = GeneralizedAdditiveModel(self$formula$get_formula_str(), fit_default = list(by = 'instant'))
                  }
                  else{
                    self$model = NULL
                  }
                },
                print = function(){
                  
                }, 
                #' @description 
                #'  Fit function of the model itself
                #' 
                #' @param model `(Any)` The model to fit
                #' @param X `(data.table | TimeseriesDT)` Timeseries data to fit. Should contains all variables in the formula
                fit_fun = function(model, X){
                  
                  X = X$get_timeseries()
                  
                  if (self$by_instant$activate){
                    return(R39Toolbox::fit(model, X))
                  }
                  else{
                    return(mgcv::gam(formula = self$formula$get_formula(), data = X))
                  }
                },
                #' @description 
                #'  Predict function of the model itself
                #' 
                #' @param model `(Any)` The model trained
                #' @param X `(data.table | TimeseriesDT)` Timeseries data to predict. 
                #' @return `(vector)` The output of the prediction of the model on X. 
                predict_fun = function(model, X){
                  
                  X = X$get_timeseries()
                  
                  if (self$by_instant$activate){
                    return(predict(model, X))
                  }
                  else{
                    return(predict(model, X) %>% unname)
                  }
                }
              )
)


#' @title GamStd (Generative Additive Model for Standard Deviation estimation)
#'
#' @docType class
#' @description
#'   The Generative Additive Model (GAM) for timeseries conditional standard deviation estimation.
#' 
#' @field formula `(formula)` the formula object representing the fit wanted (e.g y ~ temperature)
#' @field by_instant `(boolean)`. If you want to perform a fit by instant of specific granularity
#' @field granularity `(Optional[string])` `{'day', 'month'}`. The granularity used to compute instant.
#' 
#' @importFrom R6 R6Class
#' @importFrom R39Toolbox GeneralizedAdditiveModel
#' @import data.table
#' @export
GamStd = R6Class("GamStd", 
                 inherit = TimeseriesStdModel,
                 public = list(
                   initialize = function(formula = y ~ s(temperature) + s(posan) + jour_semaine + jour_ferie + ponts, by_instant = FALSE, granularity = 'day', ...){
                     
                     super$initialize(formula = formula, by_instant = by_instant, granularity = granularity, ...)
                     self$formula = Formula$new(formula)
                     
                     if (by_instant){
                       self$model = GeneralizedAdditiveModel(self$formula$get_formula_str(), fit_default = list(by = 'instant'))
                     }
                     else{
                       self$model = NULL
                     }
                   },
                   print = function(){
                     
                   }, 
                   #' @description 
                   #'  Fit function of the model itself
                   #' 
                   #' @param model `(Any)` The model to fit
                   #' @param X `(data.table | TimeseriesDT)` Timeseries data to fit. Should contains all variables in the formula
                   fit_fun = function(model, X){
                     
                     X = TimeseriesDT$new(X)$get_timeseries()
                     
                     if (self$by_instant$activate){
                       return(R39Toolbox::fit(model, X))
                     }
                     else{
                       return(mgcv::gam(formula = self$formula$get_formula(), data = X))
                     }
                   },
                   #' @description 
                   #'  Predict function of the model itself
                   #' 
                   #' @param model `(Any)`
                   #' @param X `(data.table | TimeseriesDT)` Timeseries data to predict. 
                   #' @return `(list)` The output of the prediction of the model on X. Returns the timeseries predicted (then delta = False) or the delta directly (timeseries is then the initial timeseries).
                   predict_fun = function(model, X){
                     
                     X = TimeseriesDT$new(X)$get_timeseries()
                     
                     if (self$by_instant$activate){
                       return(predict(model, X))
                     }
                     else{
                       return(predict(model, X) %>% unname)
                     }
                   }
                 )
)