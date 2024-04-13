
#' @title TimeseriesStdModel 
#' @docType class
#' @description
#'   An R6 class which stands as the model structure for each conditional standard deviation timeseries model
#'
#' @field formula `(formula)` the formula object representing the fit wanted (e.g value ~ temperature)
#' @field model `(Any)` a model object obtained such as GAM, prophet 
#' @field smoothers `(Smoother)` a Smoother object 
#' @field conditional_expectation_model `(TimeseriesModel)` the model to compute conditional expectation from
#'
#' @importFrom R6 R6Class
#' @import data.table
#' @export
TimeseriesStdModel = R6Class('TimeseriesStdModel',
                             inherit = TimeseriesModel,
                             public = list(
                               conditional_expectation_model = NULL,  
                               initialize = function(formula, by_instant, granularity, ...){
                                 
                                 super$initialize(formula, by_instant, granularity, ...)
                                 
                                 if (!'TimeseriesModel' %in% class(self$conditional_expectation_model)){
                                   stop('Please provide a TimeseriesModel object to estimate conditional expectation')
                                 }
                               }, 
                               #' @description 
                               #'  Fit the conditional variance model based on the conditional expectation one
                               #'  Relies on CV squared residuals computed from the latter to do so
                               #' 
                               #' @param outputs `(data.table | TimeseriesDT)` The output/response data
                               #' @param inputs `(data.table | TimeseriesDT)` The input data with data for both conditional
                               #' expectation and conditional variance models
                               #' @param fold_varname `(string)` the name of the variable in \code{inputs} to define CV folds from
                               fit = function(outputs, inputs, fold_varname){
                                 message('Fitting the TimeseriesStd Model.')
                                 
                                 outputs = TimeseriesDT$new(outputs, is_output = TRUE)
                                 inputs = TimeseriesDT$new(inputs) 
                                 
                                 if (! fold_varname %in% inputs$get_variables_name()){
                                   stop("You need to provide the variable definining CV folds inside the input timeseries")
                                 }
                                 
                                 message('Performing a Cross Validation prediction with conditional expectation model')
                                 output_cv_pred = self$conditional_expectation_model$cv_predict(outputs, inputs, fold_varname)
                                 output_cv_residual_sqrd = (outputs$get('y') - output_cv_pred)^2
                                 
                                 outputs$assign('y', output_cv_residual_sqrd)
                                 
                                 message('Fitting now using the residuals squared ')
                                 super$fit(outputs, inputs)
                               },
                               #' @description 
                               #'  Predict the conditional standard deviation
                               #' 
                               #' @param inputs `(data.table | TimeseriesDT)` The timeseries data to make prediction on
                               #' 
                               #' @return `(vector)` The output timeseries as a vector from the model prediction
                               predict = function(inputs){
                                 conditional_variance = super$predict(inputs)
                                 
                                 conditional_variance = pmax(0, conditional_variance)
                                 
                                 return(sqrt(conditional_variance))
                               }
                             )
)