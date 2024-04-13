#' @title GradDelta 
#'
#' @docType class
#' @description
#'   This class represents a model (GradDelta) for predicting values based on gradients, and linear models. Performs a linear model by instant.
#'
#' @field formula `(formula)` the formula object representing the fit wanted (e.g y ~ temperature)
#' @field N_min `(integer)`. Default: 30. Minimum sample allowed to perform the linear regression.
#' @field weights `(float vector)` Weights to pass in the linear regression model. Default is null.
#' @field lm `(MASS linear model)`. Default: 'robust'. Linear model. Choose between robust, least squares and ridge.
#' @field n_shift `(integer)` Default: 168. The number of steps in hour to shift your timeseries 
#' @field granularity `(Optional[string])` `{'day', 'month'}`. Default : 'day'. The granularity used to compute instant.
#' 
#' @importFrom R6 R6Class
#' @importFrom magrittr %>% %<>% set_names
#' @import data.table
#' @importFrom MASS rlm lm.gls lm.ridge
#' @export
GradDelta = R6Class("GradDelta", 
                    inherit = TimeseriesModel,
                    public = list(
                      N_min = 30,
                      gradients = list(),
                      weights = NULL,
                      lm = 'robust',
                      n_shift = 168,
                      initialize = function(formula = y ~  temperature, by_instant = FALSE , granularity = 'day',...){
                        args = list(...)
                        
                        super$initialize(formula = formula,  by_instant = by_instant, granularity = granularity, ...)
                        
                        self$formula$shift_formula()
                        
                        self$lm = rlm
                        
                        if ('lm' %in% names(args)){
                          if (args$lm == 'robust'){
                            self$lm = rlm
                          }
                          else if (args$lm == 'least squares'){
                            self$lm = lm.gls
                          }
                          else if (args$lm == 'ridge'){
                            self$lm = lm.ridge
                          }
                          else {
                            stop('Linear model asked not supported. Please choose between robust, least squares or ridge')
                          }
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
                        
                        dt = X$get_timeseries()
                        
                        if (self$by_instant$activate){
                          dt = dt %>% 
                            .[, .(coefs = list(private$fit_and_extract_coefs(.SD))), by = instant] %>% 
                            .[, unlist(coefs, recursive = FALSE), by = instant] 
                          
                          setnames(dt, old = names(dt)[-1], new = self$formula$get_explanatory_variables_formula_base())
                        }
                        else{
                          dt = dt %>% 
                            .[, .(coefs = list(private$fit_and_extract_coefs(.SD)))] %>% 
                            .[, unlist(coefs, recursive = FALSE)] 
                          
                          setnames(dt, old = names(dt), new = self$formula$get_explanatory_variables_formula_base())
                        }
                        
                        self$gradients = dt 
                        
                        return(self$gradients)
                      }, 
                      #' @description 
                      #'  Predict function of the model itself
                      #' 
                      #' @param model `(Any)` The model trained
                      #' @param X `(data.table | TimeseriesDT)` Timeseries data to predict. 
                      #' @return `(vector)` The output of the prediction of the model on X. 
                      predict_fun = function(model, X){
                        
                        X = X$get_timeseries()
                        
                        vars = self$formula$get_explanatory_variables_formula_base()
                        model = copy(model)
                        if (self$by_instant$activate){
                          X = merge.data.table(X, model, by = 'instant', suffixes = c("", "_grad"))
                        }
                        else{
                          setnames(model, old = vars, new = paste0(vars, '_grad'))
                          X = cbind(X, model)
                        }
                        
                        X[, result := 0]
                        
                        for (var in vars) {
                          var_grad = paste0(var, '_grad')
                          X[, result := result + get(var) * get(var_grad)]
                        }
                        
                        return(X$result)
                      }, 
                      #' @description
                      #'  Get gradients from the GradDelta model
                      get_gradients = function(){
                        return(self$gradients %>% copy)
                      }
                    ),
                    private = list( 
                      fit_and_extract_coefs = function(data) {
                        model = private$linear_model(data)
                        coefs = coef(model)[-1]
                        return(as.list(coefs))
                      },
                      linear_model = function(dt) {
                        if (nrow(dt) < self$N_min) {
                          warning("Not enough observations for fitting, introducing NA's")
                          return(list(coefficients = rep(NA, length(self$formula$get_explanatory_variables_formula_base()))))
                        }
                        if (is.null(self$weights)){
                          return(self$lm(self$formula$get_formula(), dt))
                        }
                        else{
                          return(self$lm(self$formula$get_formula(), dt, self$weights))
                        }
                      }
                    )
)