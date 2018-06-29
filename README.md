# docker-b3
Docker image accompanying a paper on Bayesian data analysis.

Easiest way to run this is to install Docker, give it plenty of RAM/CPU and then run:

docker run -d -p 8787:8787 -v "`pwd`":/home/rstudio/working -e PASSWORD=rstudio -e ROOT=TRUE torkar/docker-b3

then fire up the browser and point it to: localhost:8787 and user 'rstudio' as username/password.
