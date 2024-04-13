#' @title Copycat 
#'
#' @docType class
#' @description 
#'  Performs a Knn - like model.
#'
#' @importFrom R6 R6Class
#' @importFrom magrittr %>% %<>% 
#' @import data.table
#' @importFrom lubridate as_date as_datetime days
#' @export
CopyCat = R6Class("CopyCat", 
                  inherit = TimeseriesModel,
                  public = list(
                    training_set = NULL,
                    temporal_aggregator = 'day', 
                    timewindow_business_day = 30,
                    timewindow_weekend = 60,
                    initialize = function(formula = 'y ~ temperature', ...){
                      super$initialize(formula = formula, by_instant = FALSE, granularity = NULL, ...)                    },
                    print = function(){
                      
                    }, 
                    #' @description 
                    #'  Fit function 
                    #' 
                    #' @param model `(Any)`
                    #' @param X `(data.table | TimeseriesDT)` Timeseries data to fit. Should contains all variables in the formula
                    fit_fun = function(model, X){
                      self$training_set = X$get_timeseries()
                    }, 
                    #' @description 
                    #'  Predict function 
                    #' 
                    #' @param model `(Any)`
                    #' @param X `(data.table | TimeseriesDT)` Timeseries data to predict. 
                    predict_fun = function(model, X){
                      
                      variables = self$formula$get_explanatory_variables()
                      variables_comparison = paste0(variables, '_comparison')
                      variables_target = paste0(variables, '_target')
                      variables_observed = paste0(variables, '_observed')             
                      time_variables = c('day_type', 'instant', 'time')
                      
                      X$merge(self$training_set, suffixes = c('_target','_observed'))
                      ts = X$get_timeseries()[, .(time, y)]
                      
                      if (!is.null(self$temporal_aggregator)){
                        X$aggregate(granularity = self$temporal_aggregator)
                      }
                      X$compute_instant(granularity = self$temporal_aggregator)
                      X = X$add_calendar(variables = c('date', 'JourFerie', 'Ponts'))$get_timeseries()
                      
                      X %<>% 
                        .[, weekday := wday(time)] %>% 
                        .[, day_type := ifelse((weekday %% 7) > 1, 1, 0)] %>%
                        .[((jour_ferie == 1) | (ponts == 1)), day_type := 0] %>%
                        .[, date_start_comparison := time - days(ifelse(day_type == 1, self$timewindow_business_day, self$timewindow_weekend))] %>%
                        .[, date_end_comparison := time + days(ifelse(day_type == 1, self$timewindow_business_day, self$timewindow_weekend))]
                      
                      setkey(X, day_type, instant, date_start_comparison, date_end_comparison)
                      
                      X_to_compare = X[, .SD, .SDcols = c(time_variables, variables_observed)]
                      
                      setnames(X_to_compare, variables_observed, variables_comparison)
                      setkeyv(X_to_compare, time_variables)
                      
                      X_to_compare[, dummy := time]
                      
                      # Model core, normalization of variables, compute difference and get the min of the difference
                      model = foverlaps(X_to_compare, X, by.x = c(time_variables, "dummy"), nomatch = 0L)
                      model[, c(variables_comparison, variables_target) := lapply(.SD, function(x) x / max(x, na.rm = TRUE)), .SDcols = c(variables_comparison, variables_target)]
                      model[, delta_weather := apply(.SD, 1, private$abs_diff_sum, variables_target, variables_comparison)]
                      model = model[, .SD[which.min(delta_weather)], by = "time"]
                      
                      setnames(model, "i.time", "horodate_comparison")
                      model = model[, .(time, horodate_comparison)]
                      
                      ts[, date := as_date(time)]
                      ts_comparison = merge.data.table(ts, 
                                                       model, 
                                                       by.x = 'date', 
                                                       by.y = 'time') %>% 
                        .[, horodate_comparison := private$format_datetime(horodate_comparison, time)]
                      ts_comparison = merge.data.table(ts_comparison, 
                                                       ts[, .(time, y)], 
                                                       by.x = "horodate_comparison",
                                                       by.y = "time", 
                                                       all.x = TRUE, 
                                                       suffixes = c("", "_comparison")) %>% .[, .(time, y_climate_corrected=y_comparison)]  
                      
                      setorder(ts_comparison, 'time')
                      
                      return(ts_comparison)
                    }
                  ), 
                  private = list(
                    abs_diff_sum = function(row, cols_target, cols) {
                      sum(sapply(1:length(cols_target), function(i) abs(as.numeric(row[[cols_target[i]]]) - as.numeric(row[[cols[i]]]))))
                    },
                    format_datetime = function(col_compar, col){
                      return(as_datetime(glue("{col_compar} {sprintf('%02d', hour(col))}:{sprintf('%02d', minute(col))}:{sprintf('%02d', second(col))}")))
                    }
                  )
)
