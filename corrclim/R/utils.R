#' Converts a character from camelCase to snake_case.
#'
#' This function takes a character formatted in camelCase and converts it to snake_case.
#'
#' @param inputString A character character in camelCase format that needs to be converted to snake_case.
#'
#' @return `(string)` String in snake_case format.
#'
#' @export
camel_to_snake = function(inputString) {
  
  snake_case = gsub("(?<=[a-z])(?=[A-Z])", "_", inputString, perl = TRUE)
  snake_case = tolower(snake_case)
  
  return(snake_case)
}

#' Compute correlation of a 2 columns data.table
#' 
#' @param ts `(data.table)` The data table to compute the correlation on
#' 
#' @return `(float)` The correlation between the two columns
#' @export
correlation_score = function(ts){
  correlation = abs(cor(ts)[2])
  return(correlation)
}