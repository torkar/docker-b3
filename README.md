[![DOI](https://zenodo.org/badge/137405305.svg)](https://zenodo.org/badge/latestdoi/137405305)
# docker-b3
Docker image accompanying a paper on Bayesian data analysis.

Install Docker, give it plenty of RAM/CPU and run:

```docker run -d -p 8787:8787 -v "`pwd`":/home/rstudio/working -e PASSWORD="YOUR_PASSWORD" -e ROOT=TRUE torkar/docker-b3```

then fire up the browser and point it to `localhost:8787` and use `rstudio` as username and the password your set above.

**Note:** Exchange YOUR_PASSWORD to a password for you.

## Project structure

```
. 
+-- pdfs/                      # Files used to collect data from practitioners.
+-- doc/                       # Material for the analysis described in the paper.
|   +-- datasets/              # Files with CSV data.
|   +-- markdown_resourses/    # Resources for the RMarkdown source, i.e., bib/css.
|   +-- index.R                # Implementation exported from the RMarkdown file.
|   +-- index.html             # HTML report generated from the RMarkdown file.
|   +-- index.Rmd              # RMarkdown source.
|
+-- validation/                # BDA scripts for our validation.
+-- brms.R                     # Running an analysis with additional models.
+-- pt_1.1.tar.gz              # Package used for the prospective theory calculations.
```

## Where to start?

If you are interested in ...

* The code for our **analysis**, read the `index.html` file or check it out [online](https://torkar.github.io/docker-b3/).
* Checking and changing our code, open `index.Rmd` (or `index.R` for pure `R`). We recommend creating an [R Studio](https://rstudio.com/) project.
* Exploring other models and getting more details on the BDA workflow, open `brms.R`.
