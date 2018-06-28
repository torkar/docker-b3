####### Dockerfile #######
FROM rocker/rstudio
LABEL maintainer="richard.torkar@gmail.com"

ENV DEBIAN_FRONTEND noninteractive

COPY pt_1.1.tar.gz /home/rstudio/

COPY data.csv /home/rstudio/

COPY brms.R /home/rstudio/

RUN apt-get update -qq \ 
  && apt-get -y install build-essential ed pkg-config apt-utils libglu1-mesa-dev libnlopt-dev nano libgsl-dev libz-dev

RUN mkdir -p $HOME/.R/ \ 
  && echo "CXXFLAGS=-O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function -Wno-macro-redefined" >> $HOME/.R/Makevars \
  && echo "CXXFLAGS+=-flto -Wno-unused-local-typedefs" >> $HOME/.R/Makevars \
  && echo "CXXFLAGS += -Wno-ignored-attributes -Wno-deprecated-declarations" >> $HOME/.R/Makevars \
  && echo "rstan::rstan_options(auto_write = TRUE)" >> /home/rstudio/.Rprofile \
  && echo "options(mc.cores = parallel::detectCores())" >> /home/rstudio/.Rprofile

RUN install2.r --error plyr

RUN install2.r --error haven

RUN install2.r --error brms

RUN install2.r --error gridExtra

RUN install2.r --error sjstats

RUN install2.r --error ggthemes

RUN install2.r --error /home/rstudio/pt_1.1.tar.gz

RUN install2.r --error rstan 

RUN install2.r --error devtools

RUN R -e "devtools::install_github('stan-dev/bayesplot')"

RUN rm /home/rstudio/pt_1.1.tar.gz

RUN rm -rf /tmp/downloaded_packages/ /tmp/*.rds
