#' @title TimeseriesDT 
#' @docType class
#' @description
#'   An R6 class which stands as the timeseries structure described in the library
#' 
#' @field timeseries `(data.table)` A data.table which contains a time column and N value columns.
#' 
#' @importFrom R6 R6Class
#' @import data.table
#' @importFrom magrittr %>% 
#' @importFrom R39Toolbox generate_calendar
#' @importFrom lubridate as_date
#' @export
TimeseriesDT = R6Class('TimeseriesDT',
                       public = list(
                         timeseries = NULL, 
                         format_date = NULL,
                         timezone = NULL,
                         patterns = list(
                           time = c('TIME', 'DATE')),
                         initialize = function(timeseries, is_output = FALSE, format_date = "%Y-%m-%d %H:%M:%S", timezone = 'UTC'){
                           
                           self$format_date = format_date
                           self$timezone = timezone
                           
                           if ('TimeseriesDT' %in% class(timeseries)){
                             self$timeseries = timeseries$get_timeseries()
                             self$format_date = timeseries$format_date
                             self$timezone = timeseries$timezone
                           }
                           else{
                             
                             tryCatch({
                               timeseries = as.data.table(timeseries)
                             }, error = function(e) {
                               stop("Timeseries can't be formatted as data table")
                             }, finally = {
                               timeseries
                             })
                             
                             timeseries = private$rename_time_colum(timeseries)
                             
                             tryCatch({
                               
                               timeseries$time = as.POSIXct(timeseries$time) %>% 
                                 format(format = format_date) %>% 
                                 as.POSIXct(tz = timezone)
                               
                             }, error = function(e) {
                               stop(glue("TimeseriesDT has invalid column type"), conditionMessage(e))
                             }, finally = {
                               timeseries
                             })
                             
                             self$timeseries = timeseries
                           }
                           
                           if (is_output){
                             if (length(names(self$timeseries)) > 2){
                               stop('If output, timeseries should have 2 columns')
                             }
                             value_column = names(self$timeseries)[!grepl('time', names(self$timeseries))]
                             
                             setnames(self$timeseries, value_column, 'y')
                           }
                           
                         }, 
                         #' @description 
                         #'  Remove NA from timeseries data 
                         #' @param inplace `(boolean)` If TRUE or FALSE a new instance is wanted or modifiying existing one is sufficient.
                         remove_na = function(inplace = TRUE){
                           
                           dt =  copy(self$timeseries)
                           dt = na.omit(dt)
                           
                           if (inplace){
                             self$timeseries = dt
                             invisible(self)
                           }
                           else{
                             return(TimeseriesDT$new(dt))
                           }
                         },
                         #' @description
                         #'  Set the timeseries of a TimeseriesDT existing object. Use with caution.  
                         #' @param timeseries `(data.table)` 
                         set_timeseries = function(timeseries){
                           self$timeseries = timeseries
                           invisible(self)
                         },
                         #' @description 
                         #'  Get the timeseries data.table
                         #' @return `(data.table)` TimeseriesDT data.table 
                         get_timeseries = function(){
                           return(self$timeseries %>% copy)
                         }, 
                         #' @description 
                         #'  Look for patterns in a character 
                         #' @param patterns `(string vector)`
                         #' @param column `(string vector)` 
                         #' @return (boolean vector) A vector asserting is yes or no the pattern is contained in the column arg
                         check_pattern_in_column = function(patterns, column) {
                           return(grepl(paste(patterns, collapse = "|"), column))
                         }, 
                         #' @description 
                         #'  Set a new format for date  
                         #' @param format_date `(string)` A valid format for dates
                         set_format_date = function(format_date=NULL){
                           if (!is.null(format_date)){
                             self$format_date = format_date
                           }
                           self$timeseries$time = as.POSIXct(self$timeseries$time) %>% 
                             format(format = self$format_date) %>% as.POSIXct(tz = self$timezone)
                           
                           invisible(self) 
                         },
                         #' @description 
                         #'  Set a new timezone  
                         set_timezone = function(timezone){
                           self$timezone = timezone
                           self$timeseries$time = as.POSIXct(self$timeseries$time, tz = timezone) 
                           
                           invisible(self)
                         },
                         #' @description 
                         #'  Get variables from a Timeseries matching patterns
                         #' @param patterns `(string vector)`
                         get_variables_from_pattern = function(patterns){
                           return(names(self$timeseries)[self$check_pattern_in_column(toupper(patterns), toupper(names(self$timeseries)))])
                         }, 
                         #' @description 
                         #'  Get variables from the dataset.
                         get_variables_name = function(){
                           return(names(self$timeseries)[!self$check_pattern_in_column(toupper(self$patterns$time), toupper(names(self$timeseries)))])
                         },
                         #' @description 
                         #'  Get variable from timeseries. Similar as dt$var or dt[[var]]
                         get = function(var){
                           return(self$timeseries[[var]])
                         },
                         #' @description 
                         #'  Generate calendar from a timeseries 
                         #' @param variables `(string vector)`variables to add to the calendar.
                         #' @return The calendar generated
                         add_calendar = function(variables = c("date", "Annee", "Mois", "Jour", "Heure", "Minute", "Posan", "Tendance", "JourSemaine", "JourFerie", "Ponts", "isoSemaine", "isoAnnee"), inplace = TRUE){
                           timestamps = self$timeseries$time
                           
                           timestep = as.numeric(difftime(timestamps[2], timestamps[1], units="secs"))
                           
                           calendar = generate_calendar(min(timestamps),
                                                        max(timestamps),
                                                        ts=timestep, 
                                                        variables = variables) %>% 
                             as.data.table %>% 
                             set_names(sapply(names(.), camel_to_snake) %>% unname)
                           
                           variables = sapply(variables, camel_to_snake)
                           
                           if ('tendance' %in% variables){
                             calendar[, tendance := list(as.double(tendance))]
                           }
                           if ('jour_semaine' %in% variables){
                             calendar[, jour_semaine := as.factor(jour_semaine)]
                           }
                           
                           if (inplace){
                             self$timeseries %<>% merge.data.table(calendar,
                                                                   by.x="time",
                                                                   by.y="date",
                                                                   all.x = TRUE,
                                                                   all.y= FALSE)
                             invisible(self)
                           }
                           else{
                             return(calendar)
                           }
                         }, 
                         #' @description 
                         #'   Add shifted timeseries to existing timeseries
                         #' @param variables `(string vector)`variables names to shift
                         #' @param n `(integer)` number of steps to shift on
                         #' @param inplace `(boolean)` If TRUE or FALSE you want to add to existing timeseries or return a new object
                         shift = function(variables, n = 168, inplace = TRUE){
                           
                           timeseries_shifted = self$timeseries[, c(time = .(time), lapply(.SD, function(x) shift(x, n))),.SDcols = variables]
                           
                           if (inplace){
                             self$merge(timeseries_shifted, suffixes = c("", "_shifted"), inplace = inplace)
                             invisible(self)
                           }
                           else{
                             return(TimeseriesDT$new(timeseries_shifted))
                           }
                         }, 
                         #' @description
                         #'   Get the row number of the timeseries
                         nrows = function(){
                           return(nrow(self$timeseries))
                         },
                         #' @description 
                         #'   Add an instant column to existing timeseries 
                         #' @param inplace `(boolean)` If TRUE or FALSE you want to add to existing timeseries or return a new object
                         compute_instant = function(granularity = 'day', inplace = TRUE){
                           
                           if (!granularity %in% c('day', 'month')){
                             stop('Instant granularity is now available only for day and month')
                           }
                           
                           f_x = function(datetime, granularity) {
                             
                             if (granularity == 'day'){
                               return(hour(datetime) * 100 + minute(datetime))
                             }
                             else if(granularity == 'month'){
                               return(mday(datetime) * 10000 + hour(datetime) * 100 + minute(datetime))
                             }                        
                           }
                           
                           dt = copy(self$timeseries)
                           dt[, instant := f_x(time, granularity)][, instant := as.factor(instant)]
                           
                           if (inplace){
                             self$timeseries = dt
                             invisible(self)
                           }
                           else{
                             return(TimeseriesDT$new(dt))
                           }
                         }, 
                         #' @description 
                         #'   Add a suffix to explanatory variables 
                         #' @param variables `(string vector)` The variables to add suffix on
                         #' @param suffix `(string)` The suffix you want to add
                         #' @param inplace `(boolean)` If TRUE or FALSE a new instance is wanted or modifiying existing one is sufficient.
                         add_suffix = function(variables, suffix, inplace = TRUE){
                           dt = copy(self$timeseries)
                           setnames(dt, variables, paste0(variables, suffix))
                           
                           if (inplace){
                             self$timeseries = dt
                             invisible(self)
                           }
                           else{
                             return(TimeseriesDT$new(dt))
                           }
                         }, 
                         #' @description
                         #'   Sort timeseries accordin to a variable, ascending mode.
                         #' @param variable `(string)` The variable to use to order (ascending mode only) the timeseries
                         sort = function(variable, inplace = TRUE){
                           dt = copy(self$timeseries)
                           dt[order(get(variable))]
                           
                           if (inplace){
                             self$timeseries = dt
                             invisible(self)
                           }
                           else{
                             return(TimeseriesDT$new(dt))
                           }
                         },
                         #' @description 
                         #'  Compute the period start for a specific granularity
                         #' @param granularity `(string)` `{'hour', 'day', 'week', 'month', "year"}`. Aggregation level.
                         #' @param inplace `(boolean)` If TRUE or FALSE a new instance is wanted or modifiying existing one is sufficient.
                         compute_period_start = function(granularity, inplace = TRUE){
                           
                           dt = copy(self$timeseries)
                           
                           dt[, period_start := if (granularity == "hour") {
                             as.POSIXct(format(get('time'), "%Y-%m-%d %H:00:00"))
                           }
                           else if (granularity == "day") {
                             as_date(get('time'))
                           } else if (granularity == "week") {
                             as.Date(get('time')) - as.integer(format(as.Date(get('time')), "%u")) + 1
                           } else if (granularity == "month") {
                             as.Date(format(get('time'), "%Y-%m-01"))
                           } else if (granularity == "year") {
                             as.Date(format(get('time'), "%Y-01-01"))
                           }, by = .(get('time'))]
                           
                           if (inplace){
                             self$timeseries = dt
                             invisible(self)
                           }
                           else{
                             return(TimeseriesDT$new(dt))
                           }                             
                         },
                         #' @description 
                         #'   Aggregates data according to the 'time' column at different levels. Returns datetime in the time column
                         #' @param granularity `(string)` `{'hour', 'day', 'month', 'year', 'week'}` Aggregation level.
                         #' @param fun ``(callable)`` Aggregation function. 'mean' by default.
                         #' @param inplace `(boolean)` If TRUE or FALSE a new instance is wanted or modifiying existing one is sufficient.
                         aggregate = function(granularity, fun = mean, inplace = TRUE) {
                           
                           if (inplace){
                             object = self
                           }
                           else{
                             object = TimeseriesDT$new(copy(self$timeseries))
                           }
                           
                           if (!granularity %in% c('hour','day', 'month', 'year', 'week')){
                             stop("Granularity not supported, please choose between : 'hour','day', 'month', 'year', 'week'")
                           }
                           else{               
                             object$compute_period_start(granularity)
                             
                             dt = object$remove_variables('time')$get_timeseries()
                             
                             dt = dt[, lapply(.SD, fun), by = .(period_start)]
                             setnames(dt, 'period_start', 'time') 
                             
                             if (granularity == 'hour'){
                               object$set_format_date("%Y-%m-%d %H:%M:%S")
                             }
                             else{
                               object$set_format_date("%Y-%m-%d")
                             }
                           }
                           
                           if (inplace){
                             self$timeseries = dt
                             invisible(self)
                           }
                           else{
                             object$set_timeseries(dt)
                             return(object)
                           }
                         }, 
                         #' @description 
                         #'   Groupby data according to the 'time' column at different levels. Doesn't return datetime in time column 
                         #' @param granularity `(string)` `{'hour', 'wday', 'month', 'week', 'year'}`. Aggregation level.
                         #' @param fun ``(callable)`` Aggregation function. 'mean' by default.
                         #' @param inplace `(boolean)` If TRUE or FALSE a new instance is wanted or modifiying existing one is sufficient.
                         groupby = function(granularity, fun = mean){
                           if (!granularity %in% c('hour','wday', 'month', 'year', 'week')){
                             stop("Granularity not supported, please choose between : 'hour','wday', 'month', 'year', 'week'")
                           }
                           else{             
                             dt = self$timeseries  
                             
                             dt[, time := if (granularity == "hour") {
                               hour(time)
                             }
                             else if (granularity == "wday") {
                               wday(time)
                             } else if (granularity == "week") {
                               week(time)
                             } else if (granularity == "month") {
                               month(time)
                             } else if (granularity == "year") {
                               year(time)
                             }]
                             
                             dt = dt[, lapply(.SD, fun), by = time]
                             
                             return(dt)
                           }
                         },
                         #' @description 
                         #'   Select variables from timeseries
                         #' @param variables `(string vector)` The variables you want to select into the TimeseriesDT.
                         #' @param inplace `(boolean)` If TRUE or FALSE a new instance is wanted or modifiying existing one is sufficient.
                         select = function(variables, inplace = TRUE){
                           dt = copy(self$timeseries)
                           dt = dt[, .SD, .SDcols = c('time', variables)]
                           
                           if (inplace){
                             self$timeseries = dt
                             invisible(self)
                           }
                           else{
                             return(TimeseriesDT$new(dt))
                           }
                         },
                         #' @description 
                         #'   Remove duplicated values from timeseries
                         #' @param variables `(string vector)` Default : 'time'. The variables on which you want to compare uniqueness
                         #' @param inplace `(boolean)` If TRUE or FALSE a new instance is wanted or modifiying existing one is sufficient.
                         remove_duplicated = function(variables= 'time', inplace = TRUE){
                           dt = copy(self$timeseries)
                           
                           dt = unique(dt, by = variables)
                           
                           if (inplace){
                             self$timeseries = dt
                             invisible(self)
                           }
                           else{
                             return(TimeseriesDT$new(dt))
                           }
                           
                         },
                         #' @description
                         #'   A vector to your timeseries
                         #' @param name `(string)` The name of your new column
                         #' @param vector `(vector)` The vector to add to your timeseries
                         #' @param inplace `(boolean)` If TRUE or FALSE a new instance is wanted or modifiying existing one is sufficient.
                         assign = function(name, vector, inplace = TRUE){
                           dt = copy(self$timeseries)
                           dt[[name]] = vector
                           
                           if (inplace){
                             self$timeseries = dt
                             invisible(self)
                           }
                           else{
                             return(TimeseriesDT$new(dt))
                           }
                         },
                         
                         #' @description 
                         #'   Merge TimeseriesDT with an other data.table or TimeseriesDT
                         #' @param by `(string vector)` Default: 'time'. Column to merge on
                         #' @param how  `(string)` Default : 'inner'. Either inner left or all
                         #' @param suffixes `(string vector)`Suffixes to add in case of overlapping columns names
                         #' @param inplace `(boolean)` If TRUE or FALSE a new instance is wanted or modifiying existing one is sufficient.
                         merge = function(timeseries_to_merge, by = 'time', how = 'inner', suffixes = c(".x", ".y"), inplace = TRUE){
                           
                           to_merge = TimeseriesDT$new(timeseries_to_merge)$remove_duplicated()$get_timeseries()
                           dt = TimeseriesDT$new(self$timeseries)
                           dt$remove_duplicated()
                           
                           if (how == 'inner'){
                             dt = merge.data.table(self$timeseries, to_merge, by = by, suffixes = suffixes)
                           }
                           else if (how == 'left'){
                             dt = merge.data.table(self$timeseries, to_merge, by = by ,all.x = TRUE, all.y = FALSE, suffixes = suffixes)
                           }
                           else if (how == 'right'){
                             dt = merge.data.table(self$timeseries, to_merge, by = by ,all.x = FALSE, all.y = TRUE, suffixes = suffixes)
                           }
                           else if (how == 'all'){
                             dt = merge.data.table(self$timeseries, to_merge, by = by ,all.x = TRUE, all.y = TRUE, suffixes = suffixes)
                           }
                           
                           if (inplace){
                             self$timeseries = dt
                             invisible(self)
                           }
                           else{
                             return(TimeseriesDT$new(dt))
                           }
                         }, 
                         #' @description
                         #'   Remove variables from timeseries
                         #' @param variables `(string vector)` Variables to remove from timeseries
                         #' @param inplace `(boolean)` If TRUE or FALSE a new instance is wanted or modifiying existing one is sufficient.
                         remove_variables = function(variables, inplace = TRUE){
                           dt =  copy(self$timeseries)
                           dt[, c(variables) := NULL]
                           
                           if(inplace){
                             self$timeseries = dt
                             invisible(self)
                           }
                           else{
                             return(TimeseriesDT$new(dt))
                           }
                           
                         }, 
                         #' @description 
                         #'   Align timeseries to ensure same length. Returns input aligned
                         #' @param to_align `(data.table | TimeseriesDT)` The timeseries to align with
                         #' @param second_to_align `(data.table | TimeseriesDT)` A third timeseries to align with
                         align = function(to_align, second_to_align = NULL){
                           
                           to_align = TimeseriesDT$new(to_align)
                           variables = to_align$get_variables_name()
                           to_align$add_suffix(variables, '_flag_1')
                           self$merge(to_align)
                           
                           if (!is.null(second_to_align)){
                             second_to_align = TimeseriesDT$new(second_to_align)
                             second_variables = second_to_align$get_variables_name()
                             second_to_align$add_suffix(second_variables, '_flag_2')
                             self$merge(second_to_align)
                           }
                           
                           variables_flagged_1 = c('time', self$get_variables_from_pattern('_flag_1'))
                           unflagged_variables = names(self$timeseries)[!self$check_pattern_in_column('_flag', names(self$timeseries))]
                           
                           initial = self$timeseries %>% select(unflagged_variables)
                           to_align = self$timeseries %>% select(variables_flagged_1) %>% set_names(c('time', variables))
                           
                           if (!is.null(second_to_align)){
                             variables_flagged_2 = c('time', self$get_variables_from_pattern('_flag_2'))
                             second_to_align = self$timeseries %>% select(variables_flagged_2) %>% set_names(c('time', second_variables))
                             
                             self$timeseries = initial 
                             return(list(to_align = TimeseriesDT$new(to_align), 
                                         second_to_align = TimeseriesDT$new(second_to_align)))
                           }
                           else{
                             
                             self$timeseries = initial 
                             return(TimeseriesDT$new(to_align))
                           }
                         }, 
                         #' @description 
                         #'   Compute degree days using a threshold for heating or cooling days
                         #' @param temperature_column `(string)` The temperature column
                         #' @param all `(boolean)` Default: TRUE If TRUE, heating and cooling degree days are computed
                         #' @param cooling `(boolean)` If TRUE, cooling is the only to be computed
                         #' @param threshold_cooling `(float)` Default: 18. The threshold value for cooling
                         #' @param threshold_heating `(float)` Default: 15. The threshold value for heating
                         #' @param inplace `(boolean)` If TRUE or FALSE a new instance is wanted or modifiying existing one is sufficient.
                         compute_degre_days = function(temperature_column=NULL, all=TRUE, cooling=FALSE, threshold_cooling=18, threshold_heating=15, inplace = TRUE){
                           dt = copy(self$timeseries)
                           
                           if (length(names(dt)) > 2 & is.null(temperature_column)){
                             stop('Please feed temperature_columns in order to know where to compute the degre days')
                           }
                           else if (length(names(dt)) == 2 & is.null(temperature_column)) {
                             temperature_column = self$get_variables_name()
                           }
                           
                           if (all){
                             cooling = FALSE
                             dt[, c("HDD", "CDD") := .(pmax(0, threshold_heating - get(temperature_column)), 
                                                       pmax(0, get(temperature_column) - threshold_cooling))]
                           }
                           else{
                             if (cooling){
                               dt[, c("CDD") := .(pmax(0, get(temperature_column) - threshold_cooling))]
                             }
                             else{
                               dt[, c("HDD") := .(pmax(0, threshold_heating - get(temperature_column)))]
                             }
                           }
                           if (inplace){
                             self$timeseries = dt
                             invisible(self)
                           }
                           else{
                             return(Timeseries$new(dt))
                           }
                         }, 
                         #' @description 
                         #'   Returns the granularity of your timeseries in the specified unit
                         #' @param unit `(string)` The unit of time you want the granularity. Default is hour
                         get_granularity = function(unit = 'hour'){
                           return(as.numeric(difftime(self$timeseries$time[2],self$timeseries$time[1], unit = unit)))
                         },
                         #' @description 
                         #'   Renames columns
                         #' @param old_cols `(string vector)` Old columns names
                         #' @param new_cols `(string vector)` New columns names
                         #' @param inplace `(boolean)` If TRUE or FALSE a new instance is wanted or modifiying existing one is sufficient.
                         rename = function(old_cols, new_cols, inplace = TRUE){
                           dt = copy(self$timeseries)
                           setnames(dt, old_cols, new_cols)
                           
                           if (inplace){
                             self$timeseries = dt
                             invisible(self)
                           }
                           else{
                             return(TimeseriesDT$new(dt))
                           }
                         }, 
                         #' @description 
                         #'   Returns a filtered dataset 
                         #' @param y `(string)` Response variable 
                         #' @param var `(string)` Explanatory variables to filter on
                         #' @param var_shifted `(string)` Explanatory variables shifted to filter on
                         #' @param threshold `(float)` Threshold for this explanatory variables
                         #' @param q_min `(float)` Quantile minimum for filtering inputs
                         #' @param q_max `(float)` Quantile maximal for filtering inputs 
                         #' @param IC_width `(float)` Interval confidence to filder input 
                         #' @param inferior `(boolean)` If TRUE or FALSE the var has to be inferior of the threshold
                         #' @param inplace `(boolean)` If TRUE or FALSE a new instance is wanted or modifiying existing one is sufficient.
                         filter_dataset = function(y_shifted, var, var_shifted, threshold, q_max = 0.8, q_min = 0.2, IC_width = 1.5, inferior = TRUE, inplace = TRUE){
                           dt = copy(self$timeseries)
                           
                           filter_dt = dt[!is.na(get(y_shifted)) & !is.na(get(var))]
                           
                           if (inferior){
                             filter_dt %<>%
                               .[get(var) <= threshold & (get(var) - get(var_shifted)) <= threshold]
                           }
                           else{
                             filter_dt %<>%
                               .[get(var) >= threshold & (get(var) - get(var_shifted)) >= threshold]
                           }
                           
                           filter_dt %<>%
                             .[get(y_shifted) > quantile(get(y_shifted), q_min) - IC_width * (quantile(get(y_shifted), q_max) -
                                                                                                quantile(get(y_shifted), q_min)) & get(y_shifted) < quantile(get(y_shifted), q_max) + IC_width * (quantile(get(y_shifted), q_max) - quantile(get(y_shifted), q_min))]
                           
                           if (inplace){
                             self$timeseries = filter_dt
                             
                             invisible(self)
                           }
                           else{
                             return(TimeseriesDT$new(filter_dt))
                           }
                         }, 
                         #' @description
                         #'  Export your timeseries to file
                         #' @param path `(string)` A valid path to store the timeseries
                         #' @param as_data_table `(boolean)` Whether you want to export the object itself or the data.
                         #' @param format `(string)` `{'csv', 'rds'}` The format to export. Optional if self = TRUE.
                         export = function(path, as_data_table = TRUE, format = 'csv'){
                           
                           if (!as_data_table){
                             saveRDS(self, path)
                           }
                           else{
                             format = toupper(format)
                             
                             if (!grepl(format, toupper(path))){
                               stop('Format and file extension don\'t match. Please fix the path or the format expected.')
                             }
                             
                             if (format == 'CSV'){
                               fwrite(self$timeseries, path)
                             }
                             else if (format == 'RDS'){
                               saveRDS(self$timeseries, path)
                             }
                             else{
                               stop('Format not supported. Please use csv or rds.')
                             }
                           }
                         }
                       ),
                       private = list(
                         rename_time_colum = function(dt){
                           if (length(names(dt)) < 2){
                             stop(glue('Invalid data. Please feed into the model more than two columns at least'))
                           }
                           else{
                             time_column = names(dt)[self$check_pattern_in_column(self$patterns$time, toupper(names(dt)))]         
                             if (length(time_column) == 1){
                               setnames(dt, time_column, 'time')
                             }
                             else if (length(time_column) > 1){
                               # do nothing 
                             }
                             else{
                               stop('Please provide a time column')
                             }
                           }
                           return(dt)
                         }
                       )
)
