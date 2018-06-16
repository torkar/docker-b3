####### Dockerfile #######
FROM rocker/rstudio
MAINTAINER Richard Torkar richard.torkar@gmail.com

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update -qq \ 
  && apt-get -y install build-essential ed pkg-config apt-utils libglu1-mesa-dev libnlopt-dev nano

RUN mkdir -p $HOME/.R/ \ 
  && echo "CXXFLAGS=-O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function -Wno-macro-redefined" >> $HOME/.R/Makevars \
  && echo "CXXFLAGS+=-flto -Wno-unused-local-typedefs" >> $HOME/.R/Makevars \
  && echo "CXXFLAGS += -Wno-ignored-attributes -Wno-deprecated-declarations" >> $HOME/.R/Makevars \
  && echo "rstan::rstan_options(auto_write = TRUE)" >> /home/rstudio/.Rprofile \
  && echo "options(mc.cores = parallel::detectCores())" >> /home/rstudio/.Rprofile
  
RUN install2.r --error --deps TRUE rstan brms
RUN install2.r --error --deps TRUE gridExtra
RUN install2.r --error --deps TRUE sjstats
RUN rm -rf /tmp/downloaded_packages/ /tmp/*.rds
