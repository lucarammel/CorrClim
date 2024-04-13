#' @title ClimaticCorrector 
#'
#' @docType class
#' @description
#'   An R6 class which stands as the model structure for each climatic correction model
#'
#' @field timeseries_model `(TimeseriesModel)` A TimeseriesModel object instantiated.
#' @field operator `(Optional[Operator])}` An Operator object. Default is OperatorAdditive
#'
#' @importFrom R6 R6Class
#' @importFrom dplyr select
#' @import data.table
#' @export 
ClimaticCorrector = R6Class('ClimaticCorrector',
                            public = list(
                              timeseries_model = NULL, 
                              timeseries_std_model = NULL,
                              operator = NULL,
                              initialize = function(timeseries_model, ...){
                                if (! 'TimeseriesModel' %in% class(timeseries_model)){
                                  stop('Climatic model given is not of type TimeseriesModel. Please feed in a valid timeseries_model')
                                }
                                
                                self$timeseries_model = timeseries_model
                                self$operator = OperatorAdditive$new()
                                
                                args = list(...)
                                
                                if (length(args) > 0){
                                  if (is.null(names(args))){
                                    stop('Arguments passed need to be passed as keywords arguments such as operator = operator')
                                  }
                                  for (arg in names(args)){ 
                                    self[[arg]] = args[[arg]]
                                    
                                    if (arg == 'operator'){
                                      if (! 'Operator' %in% class(args[[arg]])){
                                        stop('Operator is not valid. Please feed in a valid operator, or instantiate it using : new() method')
                                      }
                                      if ('Operator2Moments' %in% class(args[[arg]])){
                                        if (is.null(args[["timeseries_std_model"]])){
                                          stop("You need to provide a timeseries_std_model to use 2-moments climatic correction")
                                        }
                                      }
                                    }
                                    else if (arg == 'timeseries_std_model'){
                                      if (! 'TimeseriesStdModel' %in% class(args[[arg]])){
                                        stop('The standard deviation model given is not of type TimeseriesStdModel. Please feed in a valid timeseries_std_model')
                                      } 
                                      if (! 'Operator2Moments' %in% class(args[['operator']])){
                                        stop('You have provided a standard deviation model. Please provide an Operator2Moments')
                                      } 
                                    }
                                  }
                                }
                              },
                              #' @description 
                              #'  Fit the model using the TimeseriesModel object
                              #' 
                              #' @param timeseries `(data.table | TimeseriesDT)` The output/response data
                              #' @param weather_observed `(data.table | TimeseriesDT)` The input weather observed
                              #' @param fold_varname `(string)` Default NULL. The variable name to use to make a cross validation. Only used for a StdModel and Operator2Moments
                              fit = function(timeseries, weather_observed, fold_varname = NULL){
                                
                                timeseries = TimeseriesDT$new(timeseries, is_output = TRUE)
                                weather_observed = TimeseriesDT$new(weather_observed)
                                
                                if ('Operator2Moments' %in% class(self$operator)){
                                  self$timeseries_std_model$fit(timeseries, weather_observed, fold_varname)
                                } 
                                self$timeseries_model$fit(timeseries, weather_observed)
                              },
                              #' @description 
                              #'  Apply the model and make the climatic correction using an Operator object
                              #' 
                              #' @param timeseries `(data.table | TimeseriesDT)` The output/response data
                              #' @param weather_observed `(data.table | TimeseriesDT)` The input weather observed
                              #' @param weather_target `(data.table | TimeseriesDT)` The input weather target
                              #' 
                              #' @return `(TimeseriesDT)` The output timeseries corrected from the weather target
                              apply = function(timeseries, weather_observed, weather_target){
                                
                                message('Applying the Climate Correction ..')
                                
                                timeseries = TimeseriesDT$new(timeseries, is_output = TRUE)
                                weather_observed = TimeseriesDT$new(weather_observed)
                                weather_target = TimeseriesDT$new(weather_target)
                                
                                out = timeseries$align(weather_target, weather_observed)
                                weather_target = out[[1]]
                                weather_observed = out[[2]]
                                
                                message('Prediction on the target :')
                                y_pred_target = self$timeseries_model$predict(weather_target)
                                
                                message('Prediction on the observed :')
                                y_pred_observed = self$timeseries_model$predict(weather_observed)
                                
                                if ('Operator2Moments' %in% class(self$operator)){
                                  message('Prediction on the target :')
                                  y_std_target = self$timeseries_std_model$predict(weather_target)
                                  
                                  message('Prediction on the observed :')
                                  y_std_observed = self$timeseries_std_model$predict(weather_observed) 
                                  
                                  y_climate_corrected = self$operator$apply(timeseries=timeseries, 
                                                                            y_pred_observed=y_pred_observed, 
                                                                            y_pred_target=y_pred_target, 
                                                                            y_std_observed=y_std_observed, 
                                                                            y_std_target=y_std_target)
                                }
                                else {
                                  y_climate_corrected = self$operator$apply(timeseries=timeseries, 
                                                                            y_pred_observed=y_pred_observed, 
                                                                            y_pred_target=y_pred_target)
                                }
                                
                                message('Climate correction ended.')
                                return(y_climate_corrected)
                              }, 
                              #' @description
                              #'  Get the operator of the Climatic Corrector
                              #' 
                              #' @return (Operator) The Operator of the Climatic Corrector
                              get_operator = function(){
                                return(self$operator)
                              }, 
                              #' @description 
                              #'  Export the ClimaticCorrector in RDS. 
                              #' @param path `(string)` Path file to write the rds file.
                              export = function(path){
                                
                                if (!grepl('RDS', toupper(path))){
                                  stop('File path should have extension .rds or .RDS')
                                }
                                
                                saveRDS(self, path)
                              }
                            )
)