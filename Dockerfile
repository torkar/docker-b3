####### Dockerfile #######
FROM rocker/rstudio
MAINTAINER Richard Torkar richard.torkar@gmail.com

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update -qq \ 
  && apt-get -y install build-essential ed pkg-config apt-utils libglu1-mesa-dev libnlopt-dev

RUN mkdir -p $HOME/.R/ \ 
  && echo "CXXFLAGS=-O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -Wno-macro-redefined \n" >> $HOME/.R/Makevars \
  && echo "CXXFLAGS+=-flto -Wno-unused-local-typedefs \n" >> $HOME/.R/Makevars \
  && echo "CXXFLAGS += -Wno-ignored-attributes -Wno-deprecated-declarations \n" >> $HOME/.R/Makevars \
  && echo "rstan::rstan_options(auto_write = TRUE)\n" >> /home/rstudio/.Rprofile \
  && echo "options(mc.cores = parallel::detectCores())\n" >> /home/rstudio/.Rprofile \
  && echo "CXXFLAGS=-O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -Wno-macro-redefined \n" >> /home/rstudio/.Rprofile \
  && echo "CXXFLAGS+=-flto -Wno-unused-local-typedefs \n" >> /home/rstudio/.Rprofile \
  && echo "CXXFLAGS += -Wno-ignored-attributes -Wno-deprecated-declarations \n" >> /home/rstudio/.Rprofile

RUN install2.r --error --deps TRUE  rstan brms \
#  gridExtra \
#  sjstats \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
