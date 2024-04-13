#' @title TimeseriesModel 
#' @docType class
#' @description
#'   An R6 class which stands as the model structure for each climatic correction model
#'
#' @field formula `(formula)` the formula object representing the fit wanted (e.g y ~ temperature)
#' @field model `(Any)` a model object obtained such as GAM, prophet 
#' @field smoothers `(MultiSmoother)` a MultiSmoother object to handle smoothing variables.
#' @field by_instant `(list)` Hyperparameter of the TimeseriesModel to take into account a fit by an instant granularity or not
#'
#' @importFrom R6 R6Class
#' @import data.table
#' @export
TimeseriesModel = R6Class('TimeseriesModel',
                          public = list(
                            formula = NULL,
                            model = NULL,   
                            smoothers = NULL,  
                            by_instant = list(activate=FALSE, granularity=NULL),
                            initialize = function(formula, by_instant, granularity, ...){
                              
                              self$formula = Formula$new(formula)
                              self$by_instant$activate = by_instant
                              self$by_instant$granularity = granularity
                              
                              args = list(...)
                              
                              if (length(args) > 0){
                                if (is.null(names(args))){
                                  stop('Arguments passed need to be passed as keywords arguments such as operator = operator, smoother = smoother')
                                }
                              }
                              
                              for (arg in names(args)){
                                if (arg == 'smoothers'){
                                  if (!'Smoother' %in% class(args[[arg]])){
                                    stop('Please provide a Smoother object.')
                                  }
                                  else{
                                    self$smoothers = args[['smoothers']]
                                  }
                                }
                                else{
                                  self[[arg]] = args[[arg]]
                                }
                              }
                            }, 
                            #' @description 
                            #'  Check if dataset in input has the variables in formula and had variables if possible.
                            #' @param X `(data.table | TimeseriesDT)` The data to test
                            #' @return X `(data.table | TimeseriesDT)` The data tested and with calendar or shifted inputs if needed.
                            check_timeseries = function(X, is_fitting = TRUE){
                              
                              tryCatch({
                                if (!'TimeseriesDT' %in% class(X)){
                                  X = TimeseriesDT$new(X)
                                }
                              }, error = function(e) {
                                stop("Input data can't be formatted as TimeseriesDT")
                              }, finally = {
                                X
                              })
                              
                              missing_vars = private$get_missing_vars(X, is_fitting)
                              
                              if (length(missing_vars) > 0) {  
                                
                                if ('instant' %in% missing_vars & self$by_instant$activate){
                                  X$compute_instant(granularity = self$by_instant$granularity)
                                } 
                                
                                if (sum(grepl('shifted', missing_vars)) >= 1){
                                  
                                  granularity = X$get_granularity(unit = 'hour')
                                  if (is_fitting){
                                    X$shift(variables = self$formula$get_all_variables_formula_base(), n = self$n_shift / granularity) # 168 in a week
                                  }
                                }
                                
                                else{
                                  X$add_calendar()
                                }
                              }
                              missing_vars = private$get_missing_vars(X, is_fitting)
                              
                              if (length(missing_vars) > 0){
                                if (! sum(grepl('shifted', missing_vars)) >= 1){
                                  stop(paste("Variables in formula not found in the dataset: ", paste(missing_vars, collapse = ", ")))
                                }
                              }
                              return(X)
                            },
                            #' @description 
                            #'  Get the formula of the model
                            get_formula = function(){
                              
                              return(self$formula$get_formula())
                            },
                            #' @description 
                            #'  Get the model itself
                            get_model = function(){
                              
                              if (!is.null(self$model)){
                                return(self$model)
                              } 
                              else{
                                return(self$get_formula())
                              }    
                            },
                            #' @description 
                            #'  Get the smoothers of the MultiSmoother object
                            get_smoothers = function(){
                              return(self$smoothers$get_smoothers())
                            }, 
                            #' @description 
                            #'  Fit the model using the TimeseriesModel object
                            #' 
                            #' @param outputs `(data.table | TimeseriesDT)` The output/response data
                            #' @param inputs `(data.table | TimeseriesDT)` The input data to fit on.
                            #' @param fold_varname `(string)` the name of the variable in \code{inputs} to define CV folds from
                            fit = function(outputs, inputs){
                              
                              message(glue('Fitting the model {class(self)[1]} ..'))
                              
                              outputs = TimeseriesDT$new(outputs, is_output = TRUE)
                              inputs = TimeseriesDT$new(inputs)
                              
                              X = outputs$merge(inputs)
                              
                              X = self$check_timeseries(X, is_fitting = TRUE)
                              
                              if (!is.null(self$smoothers)) {
                                X = self$smoothers$fit_smooth(X)
                              }
                              
                              self$model = self$fit_fun(self$model, X)
                              
                              private$set_status(1)
                              
                              message('Model fitted ! ')
                            }, 
                            #' @description 
                            #'  Predict the model and make the climatic correction using the an Operator Object
                            #' 
                            #' @param X `(data.table | TimeseriesDT)` The timeseries data to make prediction on
                            #' 
                            #' @return `(vector)` The output timeseries as a vector from the model prediction
                            predict = function(X){
                              if (private$status < 1) {
                                stop('Please fit the model before, using the fit() method.')
                              }
                              message(glue('Predicting using the model {class(self)[1]} ..'))
                              X = TimeseriesDT$new(X)
                              
                              X = self$check_timeseries(X, is_fitting = FALSE)
                              
                              if (!is.null(self$smoothers)){
                                X = self$smoothers$smooth(X)
                              }
                              
                              Y = self$predict_fun(self$model, X)
                              
                              private$set_status(2)
                              
                              return(Y)     
                            },
                            #' @description 
                            #'  Make CV predictions from the model
                            #' 
                            #' @param outputs `(data.table | TimeseriesDT)` The output/response data
                            #' @param inputs `(data.table | TimeseriesDT)` The input data
                            #' @param fold_varname `(string)` the name of the variable in \code{inputs} to define CV folds from
                            #' 
                            #' @return `(vector)` The output timeseries as a vector from the model CV prediction
                            cv_predict = function(outputs, inputs, fold_varname){
                              message('Making Cross Validation predictions')
                              
                              outputs = TimeseriesDT$new(outputs, is_output = TRUE)
                              inputs = TimeseriesDT$new(inputs)
                              
                              inputs = outputs$align(inputs)
                              
                              cv_pred = rep(NA, inputs$nrows())
                              
                              for (fold in unique(inputs$timeseries[, get(fold_varname)])){
                                message(glue('Fold : {fold}'))
                                
                                fold_mask = inputs$timeseries[, get(fold_varname)] == fold
                                
                                self$fit(outputs$timeseries[!fold_mask], inputs$timeseries[!fold_mask])
                                
                                cv_pred[fold_mask] = self$predict(inputs$timeseries[fold_mask])
                              }
                              
                              private$set_status(0) # We don't want the model to be used with the last CV fit during inference !!!
                              
                              return(cv_pred)
                            }, 
                            #' @description 
                            #'  Export the TimeseriesModel in RDS. 
                            #' @param path `(string)` Path file to write the rds file.
                            export = function(path){
                              
                              if (!grepl('RDS', toupper(path))){
                                stop('File path should have extension .rds or .RDS')
                              }
                              
                              saveRDS(self, path)
                            }
                          ), 
                          private = list(
                            status = 0, 
                            #' @description 
                            #'  Set the status of the TimeseriesModel
                            set_status = function(value){
                              
                              if (value %in% c(0,1,2)){
                                private$status = value
                              }
                              else{
                                stop("Status has to be between 0 and 2 included")
                              }
                            }, 
                            #' @description
                            #'  Get missing vars in X compared to formula
                            get_missing_vars = function(X, is_fitting){
                              if (is_fitting){
                                missing_vars = setdiff(self$formula$get_all_variables(), X$get_variables_name())
                                if (self$by_instant$activate){
                                  instant_missing = setdiff('instant', X$get_variables_name())
                                  missing_vars = c(missing_vars, instant_missing)
                                }
                              }
                              else{
                                missing_vars = setdiff(self$formula$get_explanatory_variables(), X$get_variables_name())
                                if (self$by_instant$activate){
                                  instant_missing = setdiff('instant', X$get_variables_name())
                                  missing_vars = c(missing_vars, instant_missing)
                                }
                              }
                              return(missing_vars)
                            }
                          )
)

