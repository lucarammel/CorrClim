#' @title Formula 
#' @docType class
#' @description
#'   An R6 class which stands as the formula structure of the library
#' 
#' @field formula `(str | formula)` The formula of the model. exemple : y ~ temperature + snow
#' 
#' @importFrom R6 R6Class
#' @import data.table
#' @importFrom magrittr %>% 
#' @export
Formula = R6Class('Formula',
                  public = list(
                    formula = NULL,
                    formula_base = NULL,
                    initialize = function(f){
                      
                      if ('Formula' %in% class(f)){
                        self$formula = f$get_formula()
                      }
                      else{
                        tryCatch({
                          f  = as.formula(f)
                        }, error = function(e) {
                          stop("Formula format can't be formatted as formula")
                        }, finally = {
                          self$formula = f
                        }
                        )
                      }
                      
                      if(as.character(f[[2]]) != 'y'){
                        stop('Please name the response variable : y. Don\t forget to do the same in your timeseries')
                      }
                    },
                    #' @description 
                    #'  Get the formula formatted as a character
                    #' 
                    #' @return `(string)` The formula as a character
                    get_formula_str = function(){
                      return(paste(c(as.character(self$formula)[2], as.character(self$formula)[1], as.character(self$formula)[3]), collapse = " "))
                    }, 
                    #' @description 
                    #'  Get the formula formatted 
                    get_formula = function(){
                      return(self$formula)
                    }, 
                    #' @description 
                    #'  Get the formula formatted before shift_formula method (if applied)
                    get_formula_base = function(){
                      return(self$formula_base)
                    },
                    #' @description 
                    #'  Get the explanatory variables from the base formula (before shift method)
                    get_explanatory_variables_formula_base = function(){
                      return(all.vars(self$get_formula_base())[-1] )
                    },
                    #' @description 
                    #'  Get the all variables from the base formula (before shift method)
                    get_all_variables_formula_base = function(){
                      return(all.vars(self$get_formula_base()))
                    },
                    #' @description
                    #'  Get all the variables (response and explanatory)
                    get_all_variables = function(){
                      return(all.vars(self$formula))
                    },
                    #' @description 
                    #'  Set the formula to add a character to each variables
                    shift_formula = function(){
                      
                      self$formula_base = self$formula
                      
                      response_var =  all.vars(self$get_formula_base())[1]
                      predictor_vars =  self$get_explanatory_variables_formula_base()
                      
                      response_shifted = paste0(response_var, '_shifted')
                      new_response = paste0("I(", response_var, " - ", response_shifted, ")")
                      
                      predictor_shifted = paste0(predictor_vars, '_shifted')
                      new_predictors = paste0('I(', predictor_vars, ' - ', predictor_shifted, ')')
                      
                      new_formula_str = paste(new_response, "~", paste(new_predictors, collapse = " + "))
                      
                      # Convert the character back to a formula
                      new_formula = as.formula(new_formula_str)
                      
                      self$formula = new_formula
                      
                      invisible(self)
                    }, 
                    #' @description 
                    #'  Get explanatory variables from formula
                    #' 
                    #' @return The explanatory variables from the formula
                    get_explanatory_variables = function(){
                      return(all.vars(self$formula)[-1])
                    }
                  )
)