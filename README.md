# Code to parameterize Asian carp life history 


Richard A. Erickson, David Glover, and Jahn Kallis


This code fits Asian carp observation data to different statistical models that describe the demography of the species.
We also include some simple Stan models we in the process of building our final model. 
We include these to help other because a paucity of ecological models exist for Stan.
The growth curve and length-weight regression models all include `*_clean.R` file. 
This file contains the code used for the manuscript. 
The version of the file without this ending includes extra material that may be helpful for learning how to use the model, but is not necessary for recreating our results. 

The code uses [Stan](mc-stan.org) called through [R](https://www.r-project.org/) to fit the models.
These models can readily be adapted to other fish and animal species.
Users should understand Stan and R before trying to use this code.
Additionally, we used the `data.table` package and `tidvyverse` packages to format our data.

## Code files

All of our R script call Stan models. The R scripts also manipulate and explore the data as well as examines the results.


This repository contains the following files and folder:
- `README.md`: This file.
- `LICENSE`: The standard USGS software license.
-  `lengthWeight`: This folder contains a length-weight model.
   -  `fitLW_clean.R` is the R code to fit the model used in the manuscript.
   -  `fitLW.R` is R code to fit 3 Stan models.
   -  `lwSimple.stan` is a simple linear regression with a single intercept and single slope parameter.
   -  `lwSimpleMatrix.stan` is the above model, but uses matrix notation for inputs and can include multiple slopes and intercepts. 
   -  `lengthWeight.stan` is the Stan model we used that includes a correlated structure for hyperparameters. It is described in the Stan Manual (9.13. Multivariate Priors for Hierarchical Models for Stan version 17.0). We used this model for our manuscript. 
- `maturity`: This folder contains a model to estimate the length as which female fish become sexually mature. It is a simple logistic regression.
  - `fitMaturity.R` is R code to fit the model.
  - `maturity.stan` is the Stan model.
- `vonB`: This folder contains a von Bertalanffy model. In addition to estimating the von Vertalanffy parameter, the model estimates natural mortality. We include several models because we started simple and build up to the model we used for our manuscript. 
  - `fitVonBexample.R` is R code to fit a simple, example Von B model, `vonBdemo.stan` 
  - `vonBdemo.stan` is a simple von Vertalanffy model that serves as a simple example used for debugging and learning about Stan. This model does not include a correlated coefficient structure or optimization.
  - `fitVonBnot0.R` fits the Von B model, but without a size at time zero (t0) parameter: `vonBoNot0.stan`.
  - `vonBoNot0.stan` is a Von B model without a size at time zero parameters. This model has been optimized and includes a correlated structure for coefficients.
  - `fitVonB.R` is the R code that fits the model we used in our manuscript: `vonBo.stan`
  -  `vonBo.stan` is an optimized, hierarchical model that includes correlated variables.
- `pubFigs` include code used to create the publication figures. 
  - `dataTall.R` summarizes the data (e.g., number of observations per location) and calculates the observed percentage of each sex
  - `lengthWeightPlot.R` creates the publication length-weight plots.
  - `map.R` creates the map figure used in the publication.
  - `main_river_Stems` contains a shapefile of the rivers used in the map.
  - `maturityPlot.R` contains the code used to crate the publication plot of maturity. 

## Contact for code 

Primary code developer:  Richard A. Erickson (rerickson@usgs.gov)

## Disclaimer

This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the [official USGS copyright policy](https://www2.usgs.gov/visual-id/credit_usgs.html#copyright/).


This software is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The software has not received final approval by the U.S. Geological Survey (USGS). No warranty, expressed or implied, is made by the USGS or the U.S. Government as to the functionality of the software and related material nor shall the fact of release constitute any such warranty. The software is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the software.

This software is provided "AS IS".
