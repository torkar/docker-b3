####### Dockerfile #######
FROM rocker/rstudio AS base
LABEL maintainer="richard.torkar@gmail.com"

ENV DEBIAN_FRONTEND noninteractive

COPY pt_1.1.tar.gz data.csv brms.R /home/rstudio/

COPY validation /home/rstudio/validation

COPY docs /home/rstudio/docs

FROM base AS apt-get
RUN apt-get update -qq \ 
  && apt-get -y install apt-utils libgit2-dev libssh2-1-dev libv8-dev libxml2-dev build-essential ed pkg-config apt-utils libglu1-mesa-dev libnlopt-dev nano libgsl-dev libz-dev

FROM apt-get AS r-packages
RUN mkdir -p $HOME/.R/ \ 
  && echo "CXXFLAGS=-O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function -Wno-macro-redefined" >> $HOME/.R/Makevars \
  && echo "CXXFLAGS+=-flto -Wno-unused-local-typedefs" >> $HOME/.R/Makevars \
  && echo "CXXFLAGS += -Wno-ignored-attributes -Wno-deprecated-declarations" >> $HOME/.R/Makevars \
  && echo "rstan::rstan_options(auto_write = TRUE)" >> /home/rstudio/.Rprofile \
  && echo "options(mc.cores = parallel::detectCores())" >> /home/rstudio/.Rprofile

RUN install2.r -e devtools rstan plyr bayesplot brms gridExtra haven tidyr readxl sjstats ggthemes xml2 roxygen2

FROM r-packages AS r-source

RUN install2.r -e /home/rstudio/pt_1.1.tar.gz

RUN R -e "devtools::install_github('rmcelreath/rethinking',ref='Experimental')"

RUN rm /home/rstudio/pt_1.1.tar.gz

RUN rm -rf /tmp/downloaded_packages/ /tmp/*.rds
