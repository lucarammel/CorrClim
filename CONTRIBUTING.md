# Empower Your Climate Modeling: Contribute to CorrClim!

Thank you for considering contributing to CorrClim! Contributions are essential to the success of an open-source project. This document outlines some guidelines to follow when contributing to ensure a smooth collaboration process.

## Table of Contents

1. **[Setting Up the Development Environment](#setting-up-the-development-environment)**
   - Step-by-step guide to prepare your development setup.

2. **[Code of Conduct](#code-of-conduct)**
   - Overview of the project's Code of Conduct and the expectation to abide by its terms.

3. **[Update Documentation](#update-documentations)**
   - Guidelines for maintaining and updating the project documentation.

4. **[Add Your Own Model](#add-your-own-model)**
   - Instructions for creating and integrating a new model into CorrClim.

5. **[Add Your Own Smoother](#add-your-own-smoother)**
   - Steps to develop and add a custom smoother to the project.

6. **[Add Your Own Operator](#add-your-own-operator)**
   - Process for designing and incorporating a new operator for climate data correction.

7. **[Building Tests](#build-tests)**
   - Best practices for writing unit tests to ensure reliability and functionality of new features.


## Setting Up the Development Environment

To set up the development environment, follow these steps:

1. Clone the repository: `git clone https://gitlab.pleiade.edf.fr/recoflux/corrclim`
2. Install the library in dev mode by placing the working directory where you have cloned the project. `devtools::load_all('corrclim')`

## Code of Conduct

Please note that this project is released with a [Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project, you agree to abide by its terms.

Happy contributing!

## Update and new releases

* Consider **updating docstring, notebooks** and other documentations to maintain readability of the project.

```R
devtools::document('corrclim') # Update documentation 
devtools::build_manual('corrclim') # Build the pdf manual of the project of all the documentations 
```

* Please **update [CHANGELOG](/CHANGELOG.md)** if you add new features or any fixes.
* Update then the version of the package in [DESCRIPTION](/DESCRIPTION)
* Update the download link to point the new manual of the package in line 7 of [README.md](/README.md)
  
Build the new archive of your package : 

```R
devtools::build('corrclim')
```

## Add your own model

Follow the guidelines to add your own model : 

* **Create a new file** in the /R folder. Name it `my_model.R`
* Use this structure : 

```R
#' @title MyModel
#'
#' @docType class
#' @description <MyModel description>
#'
#' @importFrom R6 R6Class
#' @import data.table
#' @export
MyModel = R6Class("MyModel", 
              inherit = TimeseriesModel,
              public = list(
                initialize = function(formula = "<default formula like : y ~ temperature>", by_instant = TRUE, granularity = 'day',...){
                  # by_instant is a Boolean and is used if you want your model to be fitted on instant of the day or the month
                  # granularity is the associated instant granularity. Meaning the instant of the day or the month.

                  # If you build a by_instant model, you will have to handle ths if / else in fit_fun and predict_fun 
                  super$initialize(..., formula = formula, by_instant = by_instant, granularity = granularity)
                  self$model = blabla # Add something here if you use a model from another package. 
                },
                #' @description 
                #'  Print my model 
                #' 
                print = function(){
                  # Add the way you want to print your model
                }, 
                #' @description 
                #'  Fit function of the model itself
                #' 
                #' @param model `(Any)` The model to fit
                #' @param X `(data.table | TimeseriesDT)` Timeseries data to fit. Should contains all variables in the formula
                fit_fun = function(model, X){
                  # Here fill the fitting part of your model
                  # Returns anything you find useful for the model.

                  # Handle the by_instant exception
                  if (self$by_instant$activate){
                    return(fit(model, X))
                  }
                  else{
                    return(fit(model, X))
                  }
                },
                #' @description 
                #'  Predict function of the model itself
                #' 
                #' @param model `(Any)` The model trained
                #' @param X `(data.table | TimeseriesDT)` Timeseries data to predict. 
                #' @return `(vector)` The output of the prediction of the model on X. 
                predict_fun = function(model, X){
                  # Make anything you want in order to predict your model.  
                  # Should returns a vector

                  # Handle the by_instant exception
                  if (self$by_instant$activate){
                    return(predict(model, X))
                  }
                  else{
                    return(predict(model, X))
                  }
                }
              )
)
```

## Add your own Smoother 

Follow the guidelines to add your own smoother : 

* In the `corrclim/R/smoother.R` file, use this structure : 

```R
#' @title  MySmoother
#'
#' @docType class
#' @description <MySmoother description> 
#' 
#' @importFrom R6 R6Class
#' @import data.table
#' @export
MySmoother = R6Class('MySmoother',
                        inherit = Smoother,
                        public = list(
                          initialize = function(...){
                            args = list(...)
                            super$initialize(...)
                          }, 
                          #' @description 
                          #'  Fit the Smoother 
                          #' 
                          #' @param timeseries `(data.table | TimeseriesDT)` The timeseries to fit on
                          #' @param y `(data.table | TimeseriesDT)` The response timeseries to compare with
                          fit_fun = function(timeseries, y = NULL){
                            # Here make the fitting part of the smoothing, if nothing to, let it empty
                          }, 
                          #' @description 
                          #'  Apply the Smoother 
                          #' 
                          #' @param timeseries `(data.table | TimeseriesDT)` The timeseries to smooth
                          #' @return `(TimeseriesDT)` The timeseries smoothed
                          smooth_fun = function(timeseries){
                            # Here it is the smoothing part. Return the timeseries smoothed with the same names for columns than the initial dataframe.
                            # Returns a TimeseriesDT
                            return(timeseries)
                          }
                        )
)

```


## Add your own Operator

Follow the guidelines to add your own operator : 

* In the `corrclim/R/operator.R` file, use this structure : 

```R

#' @title MyOperator 
#'
#' @docType class
#' @description <MyOperator description>
#' 
#' @importFrom R6 R6Class
#' @export
MyOperator = R6Class('MyOperator',
                           inherit = Operator,
                           public = list(
                             initialize = function(...){
                               super$initialize(...)
                             }, 
                             #' @description 
                             #'  Apply the Operator on the output to make the climatic correction
                             #' @param timeseries `(data.table | TimeseriesDT)` The timeseries to apply climate correction on.
                             #' @param y_pred_observed `(vector)` The inference made on the observed weather.
                             #' @param y_pred_target `(vector)` The inference made on the target weather
                             #' 
                             #' @return `(TimeseriesDT)` The output climate corrected
                             apply = function(timeseries, y_pred_observed, y_pred_target){
                               
                               return(Y_climate_corrected)
                             }
                           )
)

```

## Build tests

Please build unit tests for every feature created.
Write your new tests here : `/tests/`

```R
library(testthat)

test_that('Your Test',{
  # Make something

  # Test it 
})
```

Run all your tests with : 

```R
devtools::tests()
```

### Add examples 

Now let's show us how to use your new feature and create notebooks in Rmarkdown [here](/notebooks/) ! 