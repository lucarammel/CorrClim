# Architecture

```
CorrClim
├─ .gitignore                  - Specifies intentionally untracked files to ignore
├─ CODE_OF_CONDUCT.md          - Outlines expectations for participants' behavior and the process for reporting unacceptable behavior
├─ CONTRIBUTING.md             - Guidelines for contributing to the project
├─ DESCRIPTION                 - Metadata about the package, such as dependencies, version, author information
├─ LICENSE                     - The license under which the project is made available
├─ NAMESPACE                   - Specifies the functions made available to package users and other packages it depends on
├─ R                           - Directory containing the R source code files for the package
│  ├─ api.R                    - Defines the function to request APIs 
│  ├─ timeseries_model.R       - Base model class for climatic corrections
│  ├─ timeseries_dt.R          - Definition of the timeseries object and all built in method associated
│  ├─ climatic_corrector.R     - ClimaticCorrector class 
│  ├─ metrics.R                - Performance metrics for model evaluation
│  ├─ smoother.R               - Smoother class for smoothing terms and operations
│  ├─ operator.R               - Operator class for applying delta and compute predicted value
│  └─ utils.R                  - Utility functions for general tasks within the package
├─ README.md                   - Overview and usage instructions for the project
├─ docs                        - Documentation files and resources for the package
├─ man                         - Manual pages providing detailed documentation of functions
└─ tests                       - Directory containing test files for automated testing
   ├─ toy                      - Toy dataset
   └─ test_*.R                 - Tests for functionalities
```