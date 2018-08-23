# docker-b3
Docker image accompanying a paper on Bayesian data analysis.

Install Docker, give it plenty of RAM/CPU and then run:

docker run -d -p 8787:8787 -v "`pwd`":/home/rstudio/working -e PASSWORD=YOUR_PASSWORD -e ROOT=TRUE torkar/docker-b3

then fire up the browser and point it to: localhost:8787 and use 'rstudio' as username and the password your set above.

Note: Exchange YOUR_PASSWORD to a password for you.

Note: There should be two \` around pwd and then two " also. The two \` are not visible in some txt readers (like this on github).
