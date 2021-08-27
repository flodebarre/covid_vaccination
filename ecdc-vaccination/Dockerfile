# To build the container, run inside the app directory:
# docker build -t ecdc-vaccination .
FROM rocker/shiny-verse
USER root

# Example of adding system dependencies for a extra R packages ('mapsf' in that example)
#RUN apt-get update -qq && apt-get -y --no-install-recommends install \
#  libgdal-dev \
#  libudunits2-dev

# Example of adding extra R packages
#RUN install2.r --error --deps TRUE \
#  mapsf \
#  RColorBrewer \
#  colorspace

RUN rm -r /srv/shiny-server/* 
COPY . /srv/shiny-server/
