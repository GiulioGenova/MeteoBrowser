#FROM openanalytics/r-base
FROM rocker/r-ver:3.5.2
#MAINTAINER Tobias Verbeke "tobias.verbeke@openanalytics.eu"

# system libraries of general use
RUN apt-get update && apt-get install -y \
sudo \
pandoc \
pandoc-citeproc \
gdebi-core \
xtail \
wget \
libcurl4-gnutls-dev \
libcairo2-dev \
libxt-dev \
libssl-dev \
libssh2-1-dev \
#libssl1.0.0 \
libgit2-dev



# system library dependency for the euler app
RUN apt-get update && apt-get install -y \
libmpfr-dev \
lbzip2 \
libfftw3-dev \
libgdal-dev \
libgeos-dev \
libgsl0-dev \
libgl1-mesa-dev \
libglu1-mesa-dev \
libhdf4-alt-dev \
libhdf5-dev \
libjq-dev \
liblwgeom-dev \
libproj-dev \
libprotobuf-dev \
libnetcdf-dev \
libsqlite3-dev \
libssl-dev \
libudunits2-dev \
netcdf-bin \
protobuf-compiler \
tk-dev \
unixodbc-dev \
libv8-dev

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cloud.r-project.org/')"

# install dependencies of the app
# R dependecies
RUN sudo R -e "install.packages(c('devtools','remotes','pbapply','httr','jsonlite','geojsonio','stringr','tibble','shinyjs','dplyr','spdplyr','lubridate','shinydashboard','readr','DT','tidyr','sp','raster'))"


# install MeteoBrowser package
RUN R -e "devtools::install_github('GiulioGenova/MeteoBrowser')"

# copy the app to the image
RUN mkdir /root/MeteoBrowser
COPY /MeteoBrowser /root/MeteoBrowser

COPY Rprofile.site /usr/lib/R/etc/
  
  EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/MeteoBrowser')"]