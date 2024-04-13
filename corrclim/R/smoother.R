#' @title Smoother
#'
#' @docType class
#' @description 
#'  Base class for any smoother object.
#' 
#' @importFrom R6 R6Class
#' @export
Smoother = R6Class('Smoother',
                   public = list(
                     time_column = 'time',
                     value_column = NULL, 
                     initialize = function(...){
                       args = list(...)
                       
                       if (length(args) > 0){
                         if (is.null(names(args))){
                           stop('Arguments passed need to be passed as keywords arguments. ')
                         }
                       }
                       for (arg in names(args)){
                         self[[arg]] = args[[arg]]
                       }
                     }, 
                     #' @description 
                     #'  Fit the Smoother 
                     #' 
                     #' @param timeseries `(data.table | TimeseriesDT)` The timeseries to fit on
                     #' @param y `(data.table | TimeseriesDT)` The response timeseries to compare with
                     fit = function(timeseries, y = NULL){
                       
                       timeseries =  TimeseriesDT$new(timeseries)
                       
                       if (!is.null(y)) {
                         y = TimeseriesDT$new(y)
                         
                         if (length(y$get_variables_name()) > 1){
                           stop('The input y contains more than one variables. Please feed only one.')
                         }
                       }
                       
                       self$value_column = timeseries$get_variables_name() 
                       
                       if (length(self$value_column) > 1 & !'MultiSmoother' %in% class(self)){
                         stop('The input timeseries  should have only one variable to fit.')
                       }
                       
                       tmp = self$fit_fun(timeseries = timeseries, y = y)
                       
                       private$set_status(1)
                     }, 
                     #' @description 
                     #'  Apply the Smoother
                     #' 
                     #' @param timeseries `(data.table | TimeseriesDT)` The timeseries to smooth
                     #' @return `(TimeseriesDT)` The timeseries smoothed
                     smooth = function(timeseries){
                       if (private$status < 1){
                         stop('Please fit the smoother before')
                       }
                       
                       timeseries = TimeseriesDT$new(timeseries)
                       
                       private$set_status(2)  
                       
                       return(self$smooth_fun(timeseries)) 
                     },
                     #' @description 
                     #'  Fit and apply the smoother. Calls the fit and smooth method.
                     #' 
                     #' @param timeseries `(data.table | TimeseriesDT)` The timeseries to smooth
                     #' 
                     #' @return `(TimeseriesDT)` The timeseries smoothed
                     fit_smooth = function(timeseries, y = NULL){
                       
                       self$fit(timeseries, y)
                       
                       return(self$smooth(timeseries))
                     }, 
                     #' @description 
                     #'  Export the Smoother in RDS. 
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
                     set_status = function(x){
                       if (x %in% c(0,1,2)){
                         private$status = x
                       }
                       else{
                         stop("Status has to be between 0 and 2 included")
                       }    
                     }
                   )
)

#' @title Exponential smoother
#'
#' @docType class
#' @description 
#'  This smoother performs an exponential smooth of the timeseries. Can performs a basic exponential smoothing or a exponential smoothing for each timestep of the day, then the comparison is done on the previous day.
#' 
#' @field alpha `(float in [0,1])`. Defines as the weight of the smoothing. s(t) = alpha x(t) + (1 - alpha) s(t-1)
#' @field N `(Optional[integer])` Default: 20. Only if granularity = 'days'. The number of passed days used to compute the smoothed timeseries
#' @field granularity `(string)` `{'step', 'days'}`. Default : 'step'. If step, performs a classic EMA step by step, if days, performs a EMA by day.
#' 
#' @importFrom R6 R6Class
#' @import data.table
#' @importFrom magrittr %>% set_names
#' @importFrom TTR EMA
#' @export
ExponentialSmoother = R6Class('ExponentialSmoother',
                              inherit = Smoother,
                              public = list(
                                alpha = 0.2, 
                                N = 20,
                                granularity = 'step',
                                initialize = function(...){
                                  super$initialize(...)
                                  
                                  if(self$alpha < 0 | self$alpha > 1) {
                                    stop("alpha must be between 0 and 1")
                                  }
                                  
                                  if(!self$granularity %in% c('days', 'step')) {
                                    stop("granularity should be either days or step")
                                  }
                                }, 
                                #' @description 
                                #'  Fit the Smoother 
                                #' 
                                #' @param timeseries `(data.table | TimeseriesDT)` The timeseries to fit on
                                #' @param y `(data.table | TimeseriesDT)` The response timeseries to compare with
                                fit_fun = function(timeseries, y = NULL){
                                }, 
                                #' @description 
                                #'  Apply the Smoother
                                #' 
                                #' @param timeseries `(data.table | TimeseriesDT)` The timeseries to smooth
                                #' @return `(TimeseriesDT)`) The timeseries smoothed
                                smooth_fun = function(timeseries){
                                  
                                  timeseries = timeseries$get_timeseries()
                                  
                                  if (self$granularity == 'days'){
                                    ts = difftime(timeseries$time[2], timeseries$time[1], units = "hour") %>% as.numeric()
                                    n = as.numeric(1 / ts)
                                    
                                    if (n < 1 / 24){
                                      stop('The granularity of your timeseries is more than a day. Please provide a timeseries with hourly level to proceed the smoothing')
                                    }
                                    
                                    g = 24
                                    
                                    for (k in 0:self$N){
                                      var = glue('shifted_{k * n * g}')
                                      timeseries[, eval(var) := shift(get(self$value_column), k * n * g)] 
                                      timeseries[is.na(get(var)), eval(var) := 0]
                                    }
                                    
                                    timeseries[, smoothed := 0]
                                    
                                    for (k in 0:self$N){
                                      var_k = glue('shifted_{k * n * g}')
                                      timeseries[, smoothed := self$alpha * (1-self$alpha)^(k) * get(var_k) + smoothed]
                                    }
                                    
                                    timeseries %<>% 
                                      .[smoothed == self$alpha * get(self$value_column), smoothed := get(self$value_column)] %>%
                                      .[, .SD, .SDcols = c(self$time_column, 'smoothed')] %>% 
                                      set_names(c(self$time_column, self$value_column))
                                  }
                                  else if (self$granularity == 'step'){
                                    
                                    timeseries = EMA(timeseries, ratio = self$alpha) %>% as.data.table %>% set_names(c(self$time_column, self$value_column))
                                    
                                    first_value = timeseries[!is.na(get(self$value_column)), get(self$value_column)][1]
                                    timeseries[is.na(get(self$value_column)), eval(self$value_column) := first_value]
                                  }                           
                                  
                                  return(TimeseriesDT$new(timeseries))
                                }
                              )
)

#' @title Dummy Smoother
#'
#' @docType class
#' @description 
#'  Dummy smoother. Performs nothing.  
#' 
#' @importFrom R6 R6Class
#' @import data.table
#' @importFrom magrittr %>% set_names
#' @export
DummySmoother = R6Class('DummySmoother',
                        inherit = Smoother,
                        public = list(
                          initialize = function(...){
                            super$initialize(...)
                          }, 
                          #' @description 
                          #'  Fit the Smoother 
                          #' 
                          #' @param timeseries `(data.table | TimeseriesDT)` The timeseries to fit on
                          #' @param y `(data.table | TimeseriesDT)` The response timeseries to compare with
                          fit_fun = function(timeseries, y = NULL){
                            if (length(self$value_column) > 1){
                              stop('The input timeseries contains more than one variables. Please feed only one or use MultiSmoother.')
                            }
                          }, 
                          #' @description 
                          #'  Apply the Smoother 
                          #' 
                          #' @param timeseries `(data.table | TimeseriesDT)` The timeseries to smooth
                          #' @return `(TimeseriesDT)`) The timeseries smoothed
                          smooth_fun = function(timeseries){
                            return(timeseries)
                          }
                        )
)

#' @title GridSearch Exponential Smoother 
#'
#' @docType class
#' @description 
#'  Perform a Grid search on a Smoother to find best params and smooth using best smoother.
#' 
#' @field grid `(list)` The list of the parameters and the sequences associated to test. Example : list(alpha = seq(0.5, 1, 0.01))
#' @field score `(callable)` Default : correlation_score . A callable to compute the score to maximize. Should return a float and take a data.table with two columns in input.
#' @field smoother `(Smoother)` A smoother object not instantiated to perform the gridsearch on.
#' 
#' @importFrom R6 R6Class
#' @import data.table
#' @export
GridSearchSmoother = R6Class('GridSearchSmoother',
                             inherit = Smoother,
                             public = list( 
                               grid = list(),
                               smoother = NULL, 
                               score = NULL,
                               best_params = NULL,
                               best_smoother = NULL,
                               initialize = function(grid, smoother, score = correlation_score, ...){ 
                                 super$initialize(grid = grid, smoother = smoother, score = score, ...)
                               }, 
                               #' @description 
                               #'  Fit the smoother
                               #' 
                               #' @param timeseries `(data.table | TimeseriesDT)` The timeseries to fit on
                               #' @param y `(data.table | TimeseriesDT)` The response timeseries to compare with
                               fit_fun = function(timeseries, y){
                                 best_score = -Inf
                                 best_params = list() 
                                 best_smoother = NULL
                                 
                                 param_combinations = as.data.table(expand.grid(grid))
                                 
                                 for(i in 1:nrow(param_combinations)) {
                                   params = param_combinations[i, ]
                                   instance = do.call(self$smoother$new, as.list(params)) 
                                   
                                   x = instance$fit_smooth(timeseries)      
                                   ts = x$merge(y)$remove_variables('time')$get_timeseries()
                                   
                                   score = self$score(ts)
                                   
                                   params_str = toString(sapply(names(params), function(name) paste(name, params[[name]], sep=" : ")))
                                   message(glue('Parameters : {params} - Score : {score}'))
                                   
                                   if (score > best_score){
                                     best_score = score
                                     best_params = params
                                     best_smoother = instance
                                   }
                                 }
                                 
                                 self$best_params = best_params
                                 self$best_smoother = best_smoother      
                                 
                               }, 
                               #' @description 
                               #'  Apply the Smoother 
                               #' 
                               #' @param timeseries `(data.table | TimeseriesDT)` The timeseries to smooth
                               #' @return `(TimeseriesDT)`) The timeseries smoothed
                               smooth_fun = function(timeseries){
                                 
                                 return(self$best_smoother$smooth(timeseries))
                               }, 
                               #' @description 
                               #'  Get the best parameters
                               #' @return `(float)` Optimal alpha
                               get_best_params = function(){
                                 return(self$best_params)
                               },
                               #' @description 
                               #' Get the best smoother
                               #' @return `(Smoother)` Best smoother
                               get_best_smoother = function(){
                                 return(self$best_smoother)
                               }
                             )
)


#' @title Bayesian Exponential Smoother 
#'
#' @docType class
#' @description 
#'  Perform a Bayesian Optimisation on a Smoother to find best params and smooth using best smoother.
#'
#' @field bounds `(list)` The list of the parameters and the bounds associated. Example : list(alpha = c(0,1))
#' @field score `(callable)` Default : correlation_score . A callable to compute the score to maximize. Should return a float and take a data.table with two columns in input.
#' @field smoother `(Smoother)` A smoother object not instantiated to perform the gridsearch on.
#' @field n_iter `(integer)` The number of iteration of the Bayesian Optimisation.
#' @field init_points `(integer)` Number of randomly chosen points to sample the target function before Bayesian Optimization fitting the Gaussian Process.
#' 
#' @importFrom R6 R6Class
#' @importFrom rBayesianOptimization BayesianOptimization
#' @import data.table
#' @export
BayesianSmoother = R6Class('BayesianSmoother',
                           inherit = Smoother,
                           public = list( 
                             bounds = list(),
                             smoother = NULL, 
                             best_params = NULL, 
                             best_smoother = NULL, 
                             score = NULL,
                             init_points = 5,
                             n_iter = 20,
                             initialize = function(bounds, smoother, score = correlation_score, ...){
                               super$initialize(smoother = smoother, score = score, bounds = bounds, ...)
                             }, 
                             #' @description 
                             #'  Fit the smoother
                             #' 
                             #' @param timeseries `(data.table | TimeseriesDT)` The timeseries to fit on
                             #' @param y `(data.table | TimeseriesDT)` The response timeseries to compare with
                             fit_fun = function(timeseries, y){
                               
                               objective = function(...) {
                                 args = list(...)
                                 instance = do.call(self$smoother$new, args) 
                                 
                                 x = instance$fit_smooth(timeseries)      
                                 ts = x$merge(y)$remove_variables('time')$get_timeseries()
                                 
                                 score = self$score(ts)
                                 
                                 return(list(Score = score, Pred = score)) 
                               }
                               
                               result = BayesianOptimization(
                                 FUN = objective,
                                 bounds = self$bounds,
                                 init_points = self$init_points,  
                                 n_iter = self$n_iter,      
                                 acq = "ucb",      
                                 kappa = 2.576,
                                 eps = 0.0,
                                 verbose = TRUE
                               )
                               
                               self$best_params = result$Best_Par
                               self$best_smoother = do.call(self$smoother$new, as.list(self$best_params))
                               self$best_smoother$fit(timeseries)
                               
                             }, 
                             #' @description 
                             #'  Apply the Smoother 
                             #' 
                             #' @param timeseries `(data.table | TimeseriesDT)` The timeseries to smooth
                             #' @return `(TimeseriesDT)` The timeseries smoothed
                             smooth_fun = function(timeseries){
                               return(self$best_smoother$smooth(timeseries))
                             }, 
                             #' @description 
                             #'  Get the optimal alpha after the fit method
                             #' @return `(float)` Optimal alpha
                             get_best_params = function(){
                               return(self$best_params)
                             }, 
                             #' @description 
                             #' Get the best smoother
                             #' @return `(Smoother)` Best smoother
                             get_best_smoother = function(){
                               return(self$best_smoother)
                             }
                           )
)


#' @title MultiSmoother 
#'
#' @docType class
#' @description 
#'  A MultiSmoother class to encapsulates the role of multiple smoothing terms. Smoothes only temperature variables, and consider one smoother for one temperature variable. 
#' 
#' @field smoothers (vector of Smoother object). The smoothers to apply.
#' @field variables (vector of strings). The variables to smooth.
#' 
#' @importFrom R6 R6Class
#' @import data.table
#' @export
MultiSmoother = R6Class('MultiSmoother',
                        inherit = Smoother,
                        public = list(
                          smoothers = NULL,
                          variables = NULL,
                          initialize = function(smoothers, variables){
                            
                            super$initialize()
                            
                            if (length(smoothers) >= 1){
                              if (!is.list(smoothers)){
                                stop('Please provide a vector of smoothers or list of smoothers')
                              }
                            }
                            
                            for (s in smoothers){
                              if (! 'Smoother' %in% class(s)){
                                stop('Input is not of type Smoother')
                              }
                            } 
                            
                            if (length(variables) != length(smoothers)){
                              stop(glue('MultiSmoother has length variables : {length(variables)} and length smoothers :{length(self$smoothers)}. Don\'t know how to handle, please provide a Smoother for each column, and in the order expected'))
                            }
                            
                            self$smoothers = smoothers
                            self$variables = variables
                          }, 
                          #' @description 
                          #'  Apply the Smoother 
                          #' 
                          #' @param timeseries `(data.table | TimeseriesDT)` The timeseries to smooth
                          #' @param y `(data.table | TimeseriesDT)` The response timeseries to compare with
                          fit_fun = function(timeseries, y){
                            
                            timeseries = timeseries$get_timeseries()
                            i = 1
                            
                            for (smoother in self$smoothers){
                              message(glue('Fitting using {class(smoother)[1]} ..'))
                              
                              X = timeseries[, .SD, .SDcols = c('time', self$variables[i])]
                              smoother$fit(X, y)
                              
                              i = i + 1
                            }
                          }, 
                          #' @description 
                          #'  Apply the Smoother 
                          #' 
                          #' @param timeseries `(data.table | TimeseriesDT)` The timeseries to smooth
                          #' @return `(TimeseriesDT)`) The timeseries smoothed
                          smooth_fun = function(timeseries){
                            
                            dt = timeseries$get_timeseries()
                            
                            timeseries_smoothed = list()
                            i = 1
                            
                            for (smoother in self$smoothers){
                              message(glue('Smoothing using {class(smoother)[1]} on {self$variables[i]}..'))
                              
                              X = dt[, .SD, .SDcols = c('time', self$variables[i])]
                              
                              timeseries_smoothed = smoother$smooth(X)
                              
                              timeseries$remove_variables(self$variables[[i]]) 
                              
                              timeseries$merge(timeseries_smoothed, how = 'left')
                              
                              i = i + 1
                            }
                            
                            return(timeseries)
                          },
                          #' @description 
                          #'  Get smoothers of the MultiSmoother object
                          get_smoothers = function(){
                            return(self$smoothers)
                          }
                        )
                        
)