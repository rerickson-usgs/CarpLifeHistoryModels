## Start from rocker's r-base or official r-base
FROM rocker/r-base:latest

## Install rstan (downloads and builds all dependencies)
RUN Rscript -e 'install.packages(c("rstan", "devtools", "lubridate"))'
#, repo = "http://cran.wustl.edu")'

## Make R the default
CMD ["R"]
