## Start from rocker's r-base or official r-base
FROM rocker/tidyverse:3.6.2

## Install rstan (downloads and builds all dependencies)
RUN Rscript -e 'install.packages(c("rstan"), repo = "http://cran.wustl.edu", dependencies = TRUE)'

##RUN apt-get update && apt-get install -y --no-install-recommends r-cran-rstan

## RUN Rscript -e 'install.packages(c("lubridate"), repo = "http://cran.wustl.edu")'

##RUN apt-get update && apt-get install -y --no-install-recommends r-cran-bh

## Make R the default
CMD ["R"]
