# Code to parameterize Asian carp life history 


Richard A. Erickson, David Glover, and Jahn Kallis


This code fits Asian carp observation data to different statistical models that describe the demography of the species.
The code uses [Stan](mc-stan.org) called through [R](https://www.r-project.org/) to fit the models.
These models can readily be adapted to other fish and animal species.
Users should understand Stan and R before trying to use this code.

## Code files

All of our R script call Stan models. The R scripts also manipulate and explore the data.

This repository contains the following files and folder:
- `README.md`: This file.
- `LICENSE`: The standard USGS software license.
-  `lengthWeight`: This folder contains a length-weight model.
   -  `fitLW.R` is R code to fit the model.
   -  `lengthWeight.stan` is the Stan model.
- `maturity`: This folder contains a model to estimate the length as which fish become sexually mature.
  - `fitMaturity.R` is R code to fit the model.
  - `maturity.stan` is the Stan model.
- `vonB`: This folder contains a von Bertalanffy model. In addition to estimating the von Vertalanffy parameter, the model estimates natural mortality.
  - `vonBdemo.stan` is a simple von Vertalanffy model that serves as a simple example used for debugging and learning about Stan.
  - `fitVonBexample.R` is R code to fit the above model.
  - `vonBo.stan` is an optimized, hierarchical model that includes correlated variables.
  - `fitVonB.R` is the R code that fits the above model.  


## Contact for code 

Primary code developer:  Richard A. Erickson (rerickson@usgs.gov)

## Disclaimer

This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the [official USGS copyright policy](https://www2.usgs.gov/visual-id/credit_usgs.html#copyright/).


This software has been approved for release by the U.S. Geological Survey (USGS). Although the software has been subjected to rigorous review, the USGS reserves the right to update the software as needed pursuant to further analysis and review. No warranty, expressed or implied, is made by the USGS or the U.S. Government as to the functionality of the software and related material nor shall the fact of release constitute any such warranty. Furthermore, the software is released on condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from its authorized or unauthorized use."

This software is provided "AS IS".
