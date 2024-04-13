# Suggestions 

Find here the suggestions for new improvements : 

* Get rid of the **R39Toolbox** dependency. To do so : 
  * Implement a GAM by instant with only `mgcv` and `data.table` in `GAM` model.
  * Implement a `generate_calendar` method in `TimeseriesDT` without using the function of the `R39Toolbox` package.
* Keep on implementing **model-specific** metrics that could be in a `summary` method of each `TimeseriesModel`. 
* Fill in the `print` method of each `TimeseriesModel`. 
* **Pass variable names** to smooth into any `Smoother`, instead of forcing to have by one smoother.
