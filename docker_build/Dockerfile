## Start from rocker's r-base or official r-base
FROM rocker/r-base:latest

## Install rstan (downloads and builds all dependencies)
# RUN Rscript -e 'install.packages(c("rstan", "devtools", "lubridate"), repo = "http://cran.wustl.edu")'

RUN apt-get update && apt-get install -y --no-install-recommends r-cran-rstan
RUN Rscript -e 'install.packages(c("lubridate"), repo = "http://cran.wustl.edu")'

## Make R the default
CMD ["R"]
