#' @title Metrics 
#'
#' @docType class
#' @description 
#'  Metrics model-agnostic
#'
#' @importFrom R6 R6Class
#' @importFrom magrittr %>% %<>% 
#' @import plotly
#' @import data.table
#' @importFrom MLmetrics MAPE RMSE
#' @export
Metrics = R6Class("Metrics", 
                  public = list(
                    colors =  c(
                      "#2ca02c",  # cooked asparagus green
                      "#d62728",  # brick red
                      "#1f77b4",  # muted blue
                      "#ff7f0e",  # safety orange
                      "#9467bd",  # muted purple
                      "#8c564b",  # chestnut brown
                      "#e377c2",  # raspberry yogurt pink
                      "#7f7f7f",  # middle gray
                      "#bcbd22",  # curry yellow-green
                      "#17becf"   # blue-teal
                    ),
                    initialize  = function(){
                    },                 
                    #' @description
                    #'  Plot timeseries before and after the climate correction
                    #' 
                    #' @param timeseries `(data.table | TimeseriesDT)` timeseries with measures
                    #' @param prediction `(data.table | TimeseriesDT)` timeseries with forecasts/climate corrections
                    #' @param granularity `(string)` The granularity to aggregate on. Choose between hour, days, week, month, year
                    #' @param fun `(callable)` The aggregation function to apply.
                    #' @param ts_to_compare_with `(data.table | TimeseriesDT)` The timeseries to compare with. Builds a subplots if feeded.
                    #' @param ylabel `(string)` The label for y axis.
                    #' @param ylabel_to_compare `(string)` The label for the y axis timeseries to compare
                    #' @param title `(string)` Title of the graph
                    #' @param variables_to_compare (vector of string) The variables in the ts_to_compare_with data to plot.
                    #' @param legend (vector of string) The legend for timeseries and prediction args
                    #' @param legend_difference `(string)` The legend for the difference between timeseries and prediction
                    #' @param width `(string)` The width of the graph
                    #' @param height `(integer)` The height of the graph
                    #' 
                    #' @return `(plotly graph)` The before/after timeseries
                    plot_timeseries = function(timeseries,
                                               prediction,
                                               ts_to_compare_with=NULL,
                                               variables_to_compare=NULL,
                                               granularity = 'day', 
                                               fun = mean, 
                                               ylabel = 'Timeseries', 
                                               ylabel_to_compare = NULL,
                                               title = 'Timeseries comparison with and without climate correction',
                                               legend = c('Timeseries input', 'Timeseries climate corrected'),
                                               legend_difference = 'Correction climatique', 
                                               width = 1000,
                                               height = 700){
                      
                      timeseries = TimeseriesDT$new(timeseries, is_output=TRUE)
                      prediction = TimeseriesDT$new(prediction, is_output=TRUE)
                      dt = timeseries$merge(prediction, suffixes = c('','_climate_corrected'))
                      
                      if(!is.null(ts_to_compare_with)){
                        ts_to_compare = TimeseriesDT$new(ts_to_compare_with)
                        
                        if ('y' %in% ts_to_compare$get_variables_name() | 'y_climate_corrected' %in% ts_to_compare$get_variables_name()){
                          stop('Arg ts_to_compare_with should not contain columns named y or y_climate_corrected')
                        }
                        
                        dt$merge(ts_to_compare)
                      }
                      
                      if (!is.null(granularity)){
                        dt = dt$aggregate(granularity = granularity, fun = fun)
                      }
                      
                      dt = dt$get_timeseries()
                      
                      fig = plot_ly(dt, type = 'scatter', mode = 'lines', width = width, height = height) %>%
                        add_trace(x = ~time, y = ~y, name = legend[1], line=list(color='blue')) %>%
                        add_trace(x = ~time, y = ~y_climate_corrected, name = legend[2], line=list(color='red')) %>%
                        layout(title = title,
                               legend=list(title=list(text='Legend')),
                               xaxis = list(title = 'Time'),
                               yaxis = list(title = ylabel),
                               plot_bgcolor='#e5ecf6')
                      
                      dt = dt[, difference := y_climate_corrected - y]
                      
                      fig_difference = plot_ly(dt, type = 'scatter', mode = 'lines', fill = 'tonexty', width = width, height = height) %>%
                        add_trace(x = ~time, y = ~difference, name = legend_difference, line=list(color='#bcbd22"')) %>%
                        layout(legend=list(title=list(text='Legend')),
                               xaxis = list(title = 'Time'),
                               yaxis = list(title = legend_difference),
                               plot_bgcolor='#e5ecf6')
                      
                      if (!is.null(ts_to_compare_with)){
                        fig_to_compare = plot_ly(dt, type = 'scatter', mode = 'lines', width = width, height = height) 
                        
                        tmp = lapply(1:length(variables_to_compare), function(i) fig_to_compare  <<- fig_to_compare %>% 
                                       add_trace(x = ~time, y = ~.data[[variables_to_compare[i]]], 
                                                 name = variables_to_compare[[i]], 
                                                 line = list(color = self$colors[i])))
                        
                        fig_to_compare %<>% layout(title = title,
                                                   legend=list(title=list(text='Legend')),
                                                   xaxis = list(title = 'Time'),
                                                   yaxis = list(title = ylabel_to_compare),
                                                   plot_bgcolor='#e5ecf6')
                        
                        fig = subplot(fig, fig_difference, fig_to_compare, nrows = 3, shareX = TRUE, titleY = TRUE, titleX = TRUE) %>% 
                          layout(xaxis=list(rangeslider = list(visible = T)))
                      }
                      else{
                        fig = subplot(fig, fig_difference, nrows = 2, shareX = TRUE, titleY = TRUE, titleX = TRUE) %>% 
                          layout(xaxis=list(rangeslider = list(visible = T)))
                      }
                      
                      return(fig)
                    }, 
                    #' @description
                    #'  Root Mean Squared Error
                    #' 
                    #' @param timeseries `(data.table | TimeseriesDT)` timeseries with measures
                    #' @param prediction `(data.table | TimeseriesDT)` timeseries with forecasts/climate corrections
                    #' @param plot `(boolean)` Whether to plot or not the evolution of RSE
                    #' @param granularity `(string)` The granularity to aggregate the timeseries on (used only for plots)
                    #' @param fun `(callable)` An aggregation function for the timeseries (used only for plots)
                    #' @param width `(integer)` The width of the graph
                    #' @param height `(integer)` The height of the graph
                    #' 
                    #' @return `(float | plotly graph)` RMSE plot or RMSE float
                    RMSE = function(timeseries, 
                                    prediction, 
                                    plot = TRUE, 
                                    granularity = 'day', 
                                    fun = mean, 
                                    width = 1000, 
                                    height = 700){
                      
                      timeseries = TimeseriesDT$new(timeseries, is_output=TRUE)
                      prediction = TimeseriesDT$new(prediction, is_output=TRUE)
                      
                      dt = timeseries$merge(prediction, suffixes = c('','_climate_corrected'))$get_timeseries()
                      
                      result = RMSE(dt$y, dt$y_climate_corrected)
                      
                      if (plot){
                        error = dt[, .(time, SE = (y_climate_corrected - y) ** 2)]
                        if (!is.null(granularity)){
                          error = TimeseriesDT$new(error)$aggregate(granularity = granularity, fun = fun)$get_timeseries()
                        }
                        error = error[, .(time, RMSE = sqrt(SE))]
                        
                        fig = plot_ly(data=error, 
                                      x=~time, 
                                      y=~RMSE, 
                                      type='scatter', 
                                      mode='lines', 
                                      width = width, 
                                      height = height, 
                                      line=list(color = 'red')) %>%
                          layout(title='Root mean squared error',
                                 xaxis=list(title='Time'),
                                 yaxis=list(title='RMSE'), 
                                 showlegend = FALSE, 
                                 plot_bgcolor='#e5ecf6') %>%
                          add_annotations(
                            text=glue("RMSE : {round(result,2)}"),  
                            x=0.2, y=0.8,              
                            font=list(
                              family="Arial, sans-serif",
                              size=16,
                              color="black"
                            ), 
                            showarrow = FALSE,
                            xref = "paper",
                            yref = "paper",
                            xanchor = "center",
                            yanchor = "bottom"
                          )
                        return(fig)
                      }
                      return(result)
                    }, 
                    #' @description
                    #'  Mean Absolute Pourcentage Error
                    #' @param timeseries `(data.table | TimeseriesDT)` timeseries with measures
                    #' @param prediction `(data.table | TimeseriesDT)` timeseries with forecasts/climate corrections
                    #' @param plot `(boolean)` Whether to plot or not the evolution of APE
                    #' @param granularity `(string)` The granularity to aggregate the timeseries on (used only for plots)
                    #' @param fun `(callable)` An aggregation function for the timeseries (used only for plots)
                    #' @param width `(integer)` The width of the graph
                    #' @param height `(integer)` The height of the graph
                    #' 
                    #' @return `(float | plotly graph)` MAPE plot or MAPE float
                    MAPE = function(timeseries, 
                                    prediction, 
                                    plot = TRUE,
                                    granularity = 'day', 
                                    fun = mean,
                                    width = 1000, 
                                    height = 700){
                      
                      timeseries = TimeseriesDT$new(timeseries, is_output=TRUE)
                      prediction = TimeseriesDT$new(prediction, is_output=TRUE)
                      
                      dt = timeseries$merge(prediction, suffixes = c('','_climate_corrected'))$get_timeseries()
                      
                      result = MAPE(dt$y, dt$y_climate_corrected)
                      
                      if (plot){
                        error = dt[, .(time, APE = abs(100 * (y_climate_corrected - y) / y ))]
                        if (!is.null(granularity)){
                          error = TimeseriesDT$new(error)$aggregate(granularity = granularity, fun = fun)$get_timeseries()
                        }
                        
                        fig = plot_ly(data=error, 
                                      x=~time, 
                                      y=~APE, 
                                      type='scatter', 
                                      mode='lines', 
                                      width = width,
                                      height = height, 
                                      line=list(color = 'red')) %>%
                          layout(title='Mean absolute percentage error',
                                 xaxis=list(title='Time'),
                                 yaxis=list(title='MAPE (%)'), 
                                 showlegend = FALSE, 
                                 plot_bgcolor='#e5ecf6') %>%
                          add_annotations(
                            text=glue("MAPE : {round(result * 100, 2)}%"),  
                            x=0.2, y=0.8,              
                            font=list(
                              family="Arial, sans-serif",
                              size=16,
                              color="black"
                            ), 
                            showarrow = FALSE,
                            xref = "paper",
                            yref = "paper",
                            xanchor = "center",
                            yanchor = "bottom"
                          )
                        return(fig)
                      }
                      return(result)
                    }, 
                    #' @description
                    #'  Bias (error = measure - forecast/cc)
                    #' @param timeseries `(data.table | TimeseriesDT)` timeseries with measures
                    #' @param prediction `(data.table | TimeseriesDT)` timeseries with forecasts/climate corrections
                    #' @param plot `(boolean)` Whether to plot or not the evolution of APE
                    #' @param granularity `(string)` The granularity to aggregate the timeseries on (used only for plots)
                    #' @param fun `(callable)` An aggregation function for the timeseries (used only for plots)
                    #' @param width `(integer)` The width of the graph
                    #' @param height `(integer)` The height of the graph
                    #' 
                    #' @return `(float | plotly graph)` BIAS plot or BIAS float
                    BIAS = function(timeseries, 
                                    prediction, 
                                    plot = TRUE, 
                                    granularity = 'day', 
                                    fun = mean,
                                    width = 1000, 
                                    height = 700){
                      
                      timeseries = TimeseriesDT$new(timeseries, is_output=TRUE)
                      prediction = TimeseriesDT$new(prediction, is_output=TRUE)
                      
                      dt = timeseries$merge(prediction, suffixes = c('','_climate_corrected'))$get_timeseries()
                      
                      result = mean(dt$y - dt$y_climate_corrected)
                      
                      if (plot){
                        error = dt[, .(time, E = y - y_climate_corrected)]
                        if (!is.null(granularity)){
                          error = TimeseriesDT$new(error)$aggregate(granularity = granularity, fun = fun)$get_timeseries()
                        }
                        
                        fig = plot_ly(data=error, 
                                      x=~time, 
                                      y=~E, 
                                      type='scatter', 
                                      mode='lines', 
                                      width = width, 
                                      height = height,  
                                      line=list(color = 'red')) %>%
                          layout(title='Bias',
                                 xaxis=list(title='Time'),
                                 yaxis=list(title='Bias'), 
                                 showlegend = FALSE, 
                                 plot_bgcolor='#e5ecf6') %>%
                          add_annotations(
                            text=glue("Bias : {round(result * 100, 2)}"),  
                            x=0.2, y=0.8,              
                            font=list(
                              family="Arial, sans-serif",
                              size=16,
                              color="black"
                            ), 
                            showarrow = FALSE,
                            xref = "paper",
                            yref = "paper",
                            xanchor = "center",
                            yanchor = "bottom"
                          )
                        return(fig)
                      }
                      return(result)
                    }, 
                    #' @description
                    #'  Plot the volume of climate correction aggregated on a specific granularity
                    #' @param timeseries `(data.table | TimeseriesDT)` timeseries with measures
                    #' @param prediction `(data.table | TimeseriesDT)` timeseries with forecasts/climate corrections
                    #' @param granularity `(string)` The granularity to aggregate the timeseries on (used only for plots)
                    #' @param fun `(callable)` An aggregation function for the timeseries (used only for plots)
                    #' @param ylabel `(string)` The label for y axis.
                    #' @param title `(string)` The graph title
                    #' @param width `(integer)` The width of the graph
                    #' @param height `(integer)` The height of the graph
                    #' 
                    #' @return `(plotly graph)` Barplot of the climate correction volume
                    plot_bar_volume_correction = function(timeseries,
                                                          prediction,
                                                          granularity = 'month', 
                                                          fun = mean, 
                                                          title = 'Climate correction volume', 
                                                          ylabel = 'Energy', 
                                                          width = 1000, 
                                                          height = 700){
                      
                      timeseries = TimeseriesDT$new(timeseries, is_output=TRUE)
                      prediction = TimeseriesDT$new(prediction, is_output=TRUE)
                      
                      dt = timeseries$merge(prediction, suffixes = c('','_climate_corrected'))$groupby(granularity = granularity, fun = fun)
                      
                      volume = dt[, .(time, volume_climate_corrected = y_climate_corrected - y)]
                      
                      fig = plot_ly(width=width, height = height) %>%
                        add_trace(data=volume, x=~time , y=~volume_climate_corrected, type='bar', name="Climate correction volume") %>%
                        layout(
                          title=title,
                          xaxis=list(title='Time'),
                          yaxis=list(title=ylabel), 
                          showlegend = FALSE, 
                          plot_bgcolor='#e5ecf6')
                      
                      return(fig)
                    }, 
                    #' @description
                    #'  Plot the evolution through time of climate correction aggregated at a specific granularity, for different categories
                    #' @param prediction (data.table | TimeseriesDT | list there of) list of timeseries with forecasts/climate corrections (1 element for each category)
                    #' @param granularity `(string)` The granularity to aggregate the timeseries on (used only for plots)
                    #' @param fun `(callable)` An aggregation function for the timeseries (used only for plots)
                    #' @param ylabel `(string)` The label for y axis.
                    #' @param title `(string)` The graph title
                    #' @param width `(integer)` The width of the graph
                    #' @param height `(integer)` The height of the graph
                    #' 
                    #' @return `(plotly graph)` Barplot of the climate correction volume
                    plot_aggregated_cc_evol_by_category= function(prediction,
                                                                  category_varname,
                                                                  granularity = 'year', 
                                                                  fun = sum, 
                                                                  title = 'Climate correction evolution by category', 
                                                                  ylabel = 'Energy', 
                                                                  width = 1000, 
                                                                  height = 700){
                      
                      if (!is.list(prediction)){
                        prediction = list(prediction)
                      }
                      
                      prediction = lapply(prediction, function(p){TimeseriesDT$new(p)})
                      prediction = lapply(prediction, function(p){p$aggregate(granularity = granularity, fun = fun)$get_timeseries()})
                      
                      fig = plot_ly(width=width, height = height) %>%
                        add_trace(data=prediction[[1]], 
                                  x=~time , 
                                  y=~y_climate_corrected, 
                                  type='bar', 
                                  name=names(prediction)[1]) %>%
                        layout(
                          title=title,
                          xaxis=list(title='Time'),
                          yaxis=list(title=ylabel), 
                          showlegend = FALSE, 
                          plot_bgcolor='#e5ecf6')
                      
                      if (length(prediction) > 1){
                        for (i in 2:length(prediction)){
                          fig %<>% add_trace(data=prediction[[i]], x=~time , y=~y_climate_corrected, type='bar', name=names(prediction)[i])
                        }
                      }
                      
                      return(fig)
                    }, 
                    #' @description
                    #'  Plot a scatter of y_ts vs x_ts. Add second_y_ts if you want two scatter group.
                    #' @param y_ts `(data.table | TimeseriesDT)` The timeseries to plot in y axis
                    #' @param second_y_ts `(data.table | TimeseriesDT)` The second timeseries to plot in y axis
                    #' @param x_ts `(data.table | TimeseriesDT)` The timeseries to plot in x axis
                    #' @param granularity `(string)` The granularity to aggregate the timeseries 
                    #' @param fun `(callable)` An aggregation function for the timeseries
                    #' @param ylabel `(string)` The label for y axis
                    #' @param xlabel `(string)` The label for x axis
                    #' @param title `(string)` The graph title
                    #' @param width `(integer)` The width of the graph
                    #' @param height `(integer)` The height of the graph
                    #' @param size `(integer)` Dot size of the scatter plot
                    #' 
                    #' @return `(float | plotly graph)` Scatter plot for timeseries
                    plot_scatter_timeseries = function(y_ts, 
                                                       x_ts, 
                                                       xlabel, 
                                                       ylabel, 
                                                       title, 
                                                       second_y_ts = NULL, 
                                                       legend = c('First Timeseries', 'Second Timeseries'),
                                                       opacity = 0.7,
                                                       granularity = 'day',
                                                       width = 1000, 
                                                       height = 700,
                                                       size = 6,
                                                       fun = mean){
                      
                      x_ts = TimeseriesDT$new(x_ts, is_output = TRUE)
                      y_ts = TimeseriesDT$new(y_ts, is_output = TRUE)
                      
                      y_ts = y_ts$merge(x_ts, suffixes = c('', '_weather'))$aggregate(granularity = granularity, fun = fun)
                      
                      if (is.null(second_y_ts)){
                        y_ts = y_ts$get_timeseries()
                        fig = plot_ly(data = y_ts, width = width, height = height) %>% 
                          add_trace(x = ~y_weather, y = ~y, type = 'scatter', mode = 'markers', marker = list(size = size, 
                                                                                                              color = self$colors[1],
                                                                                                              opacity = opacity)) %>% 
                          layout(title = title,
                                 xaxis = list(title = xlabel),
                                 yaxis = list(title = ylabel), 
                                 plot_bgcolor='#e5ecf6')
                      }
                      else{
                        second_y_ts = TimeseriesDT$new(second_y_ts, is_output = TRUE)
                        second_y_ts =second_y_ts$aggregate(granularity = granularity, fun = fun)$rename('y', 'y_cc')
                        
                        y_ts = y_ts$merge(second_y_ts)$get_timeseries()
                        
                        fig = plot_ly(data = y_ts, width = width) %>% 
                          add_trace(type= 'scatter', x = ~y_weather, y = ~y, mode = 'markers', name = legend[1], marker = list(size = size, 
                                                                                                                               color = self$colors[1],
                                                                                                                               opacity = opacity)) %>% 
                          add_trace(type = 'scatter', x = ~y_weather, y = ~y_cc, mode = 'markers', name = legend[2], marker = list(size = size, 
                                                                                                                                   color = self$colors[2], 
                                                                                                                                   opacity = opacity)) %>%
                          layout(title = title,
                                 xaxis = list(title = xlabel),
                                 yaxis = list(title = ylabel), 
                                 plot_bgcolor='#e5ecf6')
                      }                      
                      return(fig)
                    }
                  )
)

