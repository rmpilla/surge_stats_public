# Background

Code to reproduce "A national-scale model for spatially-explicit predictions of summertime methane emissions from reservoir water surfaces" by Rachel M. Pilla, Bridget D. Deemer, and Natalie A. Griffiths.

## Version Control

This project uses git version control. git software is independent from the github website and must be installed on each users computer. Users may submit pull requests if they wish to contribute to the project.

## Library Management

This project uses the `renv` library to manage library versions. After creating a local clone, a message will appear in the console directing users to sync the project library by running `renv::restore()`. This will download the packages and versions used in this project as specified in the renv.lock file. Please see here (<https://rstudio.github.io/renv/index.html>) for a primer on `renv` and here (<https://rstudio.github.io/renv/articles/collaborating.html>) for details.

Please use caution when updating libraries already captured in the lock file. The code runs successfully with the suite of packages captured in the current lock file; updating package versions could cause problems with the existing code. The whole point of `renv` is to minimize the potential for code breakage due to differences among package versions.

The R project was initiated using R.4.5.0 and `renv` will produce a warning message if a different version of R is used. In theory, `renv` shouldn't be sensitive to the version of R, but in practice I have found that `renv::restore()` can take a very long time or even fail if a different version of R is used.

Please note that the .Rprofile file is under version control in the repository. This file contains an autoloader which automatically downloads and installs the appropriate version of `renv` into the project library. It is best that users do not use the .Rprofile file to further customize the environment.

Finally, `renv::restore()` may require Rtools. Rtools can be found here (<https://cran.r-project.org/bin/windows/Rtools/history.html>). Be sure to install a version that is compatible with your R version.

## Conflicted

This project uses `conflicted` to manage conflicts among functions with the same names, but in different packages. `conflicted` preferences are defined in the .Rprofile file and will be imposed when the R session is initiated.

## Targets

The computational workflow is controlled by the `targets` package and is defined in the \_targets.R file. Run `targets::vis_network` to generate an overview of the `targets` pipeline. `targets::tar_make` will ensure the targets are up to date. See the `targets` user manual for details.

## Directory Structure

The entire manuscript can be reproduced by code in the `R` directory. The folder contains scripts that define functions used in the `targets` pipeline. The manuscript is written in SuRGE_stats_paper.qmd and SuRGE_supplementary_doc.qmd. The functions and .qmd files are run as defined in \_targets.R. All data required by the functions are in the input_data folder.

## Reproducing the Manuscript

1.  Clone or fork the repository.\
2.  Run `renv::restore()` in the console (see details above).\
3.  Run `targets::tar_make()`. This function will execute all data analysis, create figures and tables, and knit the manuscript as .docx files.

## Using the Regression Tree Models

After running `targets::tar_make()`, the regression tree models can be loaded into the workspace by running `targets::tar_load(random_forest_object)`. `random_forest_object` is a list containing several random forest models created using `spmodel::splmRF` which runs `ranger::ranger` in the background. Users are advised to utilize the `ranger` element nested within each list element for making predictions (see manuscript for a discussion). Predictions for reservoirs of interest can be obtained by feeding a dataframe of predictor values into the ranger object using the `predict` function.
