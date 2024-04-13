#' @title Operator
#'
#' @docType class
#' @description 
#'  Represents the structure of Climate Correction Operator.
#' 
#' @importFrom R6 R6Class
#' @export
Operator = R6Class('Operator',
                   public = list(
                     initialize = function(...){
                       
                     }, 
                     #' @description 
                     #'  Apply the Operator on the output to make the climatic correction
                     #' @param timeseries `(data.table | TimeseriesDT)` The timeseries to apply climate correction on.
                     #' @param y_pred_observed `(vector)` The inference made on the observed weather.
                     #' @param y_pred_target `(vector)` The inference made on the target weather
                     #' @param y_std_observed `(vector)` The inference of standard deviation made on the observed weather
                     #' @param y_std_target `(vector)` The inference of standard deviation made on the target weather
                     #' 
                     #' @return `(TimeseriesDT)` The output climate corrected
                     apply = function(timeseries, y_pred_observed, y_pred_target, y_std_observed = NULL, y_std_target = NULL){
                       
                       message(glue('Applying {class(self)[1]} for climate correction'))
                       
                       timeseries = TimeseriesDT$new(timeseries)
                       
                       self$apply_fun(timeseries=timeseries, 
                                      y_pred_observed=y_pred_observed, 
                                      y_pred_target=y_pred_target, 
                                      y_std_observed=y_std_observed, 
                                      y_std_target=y_std_target)
                       
                     }
                   )
)


#' @title OperatorTarget 
#'
#' @docType class
#' @description 
#'  Operator returning model estimation at target weather as a climatic correction
#' 
#' @importFrom R6 R6Class
#' @export
OperatorTarget = R6Class('OperatorTarget',
                         inherit = Operator,
                         public = list(
                           initialize = function(...){
                             super$initialize(...)
                           }, 
                           #' @description 
                           #'  Apply function of the Target operator
                           #' 
                           #' @param y_pred_target `(data.table | TimeseriesDT)` The inference made on the target weather
                           #' @param ... Additional arguments 
                           #' 
                           #' @return `(TimeseriesDT)` The output climate corrected
                           apply_fun = function(y_pred_target, ...){
                             return(TimeseriesDT$new(y_pred_target))
                           }
                         )
)


#' @title OperatorAdditive 
#'
#' @docType class
#' @description 
#'  Additive Operator. Performs an addition of the climate part of the timeseries
#' 
#' @importFrom R6 R6Class
#' @export
OperatorAdditive = R6Class('OperatorAdditive',
                           inherit = Operator,
                           public = list(
                             initialize = function(...){
                               super$initialize(...)
                             }, 
                             #' @description 
                             #'  Apply function of the Additive operator
                             #' 
                             #' @param timeseries `(data.table | TimeseriesDT)` The timeseries to apply climate correction on.
                             #' @param y_pred_observed `(vector)` The inference made on the observed weather.
                             #' @param y_pred_target `(vector)` The inference made on the target weather
                             #' @param ... Additional arguments 
                             #' 
                             #' @return `(TimeseriesDT)` The output climate corrected
                             apply_fun = function(timeseries, y_pred_observed, y_pred_target, ...){
                               
                               timeseries = TimeseriesDT$new(timeseries)$get_timeseries()
                               
                               delta = y_pred_target - y_pred_observed
                               
                               Y_climate_corrected = timeseries[, .(time, y_climate_corrected = (y + delta))]
                               
                               return(TimeseriesDT$new(Y_climate_corrected))
                             }
                           )
)

#' @title OperatorMultiplicative 
#'
#' @docType class
#' @description 
#'  Multiplicative Operator. Apply a share of the climate part of the timeseries 
#' 
#' @importFrom R6 R6Class
#' @export
OperatorMultiplicative = R6Class('OperatorMultiplicative',
                                 inherit = Operator,
                                 public = list(
                                   initialize = function(...){
                                     super$initialize(...)
                                   }, 
                                   #' @description 
                                   #'  Apply the Multiplicative Operator 
                                   #' 
                                   #' @param timeseries `(data.table | TimeseriesDT)` The timeseries to apply climate correction on.
                                   #' @param y_pred_observed `(vector)` The inference made on the observed weather.
                                   #' @param y_pred_target `(vector)` The inference made on the target weather
                                   #' @param ... Additional arguments 
                                   #' 
                                   #' @return `(TimeseriesDT)` The output climate corrected
                                   apply_fun = function(timeseries, y_pred_observed, y_pred_target, ...){
                                     
                                     timeseries = TimeseriesDT$new(timeseries)$get_timeseries()
                                     
                                     Y_climate_corrected = timeseries[, .(time, y_climate_corrected = (y * y_pred_target / y_pred_observed))]
                                     
                                     return(TimeseriesDT$new(Y_climate_corrected))
                                   }
                                 )
)


#' @title Operator2Moments 
#'
#' @docType class
#' @description 
#'  Operator to perform climatic correction over the 2 first distribution moments (expectation and standard deviation) 
#' 
#' @importFrom R6 R6Class
#' @export
Operator2Moments = R6Class('Operator2Moments',
                           inherit = Operator,
                           public = list(
                             initialize = function(...){
                               super$initialize(...)
                             }, 
                             #' @description 
                             #'  Apply the 2-moments Operator 
                             #' 
                             #' @param timeseries `(data.table | TimeseriesDT)` The timeseries to apply climate correction on.
                             #' @param y_pred_observed `(vector)` The inference made on the observed weather.
                             #' @param y_pred_target `(vector)` The inference made on the target weather
                             #' @param y_std_observed `(vector)` The inference of standard deviation made on the observed weather
                             #' @param y_std_target `(vector)` The inference of standard deviation made on the target weather
                             #' 
                             #' @return `(TimeseriesDT)` The output climate corrected
                             apply_fun = function(timeseries, y_pred_observed, y_pred_target, y_std_observed, y_std_target){
                               
                               timeseries = TimeseriesDT$new(timeseries)$get_timeseries()
                               
                               index_no_std = y_std_target == 0 | y_std_observed == 0
                               y_std_target[index_no_std] = 1
                               y_std_observed[index_no_std] = 1
                               
                               Y_climate_corrected = timeseries[, .(time, y_climate_corrected = (y_pred_target + (y_std_target / y_std_observed) * (y - y_pred_observed)))]
                               
                               return(TimeseriesDT$new(Y_climate_corrected))
                             }
                           )
)