# Copyright Richard Torkar
#
# Analysis of validation for the paper:
# Arguing Practical Significance in Software Engineering Using Bayesian 
# Data Analysis

library(readxl)

# We'll use the experimental branch of rethinking.
# install.packages(c("coda","mvtnorm","devtools","loo"))
# library(devtools)
# devtools::install_github("rmcelreath/rethinking",ref="Experimental")
library(rethinking)
options(mc.cores = parallel::detectCores())
library(tidyr)

# Categories we should compare (column name and number):
# A (4) vs. G (10), i.e., depending on prez would you introduce ET?
# 0 = No, 1 = Yes
#
# and
#
# D (7) vs. J (13), i.e., depending on prez would you utilize more senior 
# testers?
# 0 = No, 1 = Yes
#
# We also have Column #3 which we can use as predictor (# years in the company)
# I'm very hesitant in using Experience in years (Column #1) since the numbers 
# seems strange compared to Column #3.

d <- read_xlsx("data.xlsx")

# We're comparing how subjects interpret traditional (TRAD) descriptions from 
# analysis with Bayesian analysis using cumulative prospect theory (BA). 
# We want to know if they would 
# * introduce exploratory testing (ET) or not
# * introduce more experienced (ME) testers or not
# For each of the above questions we ask how certain they are from 1 to 5.
#
# We'll make use of the columns "Years in company" and "A", "B", "D", "E", "G", 
# "H", "J", and "K". Let's rename them to something sane (at least for me :)

colnames(d)[colnames(d)=="Years in company"] <- "experience"

# The answers to if they would introduce ET presented in a traditional way
colnames(d)[colnames(d)=="A"] <- "ET_TRAD"

# Same as previous but presented using BA w/ cumulative prospect theory
colnames(d)[colnames(d)=="G"] <- "ET_BA"

# The answers to if they would introduce more experience testers presented in a 
# traditional way
colnames(d)[colnames(d)=="D"] <- "ME_TRAD"

# Same as previous but presented using BA
colnames(d)[colnames(d)=="J"] <- "ME_BA"

# Let's rename the "how certain are you of your answer" following the same 
# naming scheme as above and suffixing it with _L for Likert.
colnames(d)[colnames(d)=="B"] <- "ET_TRAD_L"
colnames(d)[colnames(d)=="H"] <- "ET_BA_L"
colnames(d)[colnames(d)=="E"] <- "ME_TRAD_L"
colnames(d)[colnames(d)=="K"] <- "ME_BA_L"

# Let's drop the rest of the columns
d <- d[-c(1,2,6,9,12,15)]

# Center and standardize our experience pedictor (mostly for the sake of 
# sampling)
d$experience_s <- standardize(d$experience)

# How many NAs?
sum(is.na(d))
# [1] 8
# So, we have 8 NAs (3 of 1/0, and 5 of 1-5), which we'll need to handle in a 
# principled way, i.e., since they are in the outcome variables we can simply 
# remove those cells later.
################################################################################
# First let's design a model for the question if we want to use ET (either when
# presented in the traditional way, or in the way we propose)

# Make the data long format
data_long <- gather(d, treatment, measurement, ET_TRAD, ET_BA)

# We need to build models where we use logistic regression (0/1, 
# i.e., Binomial) and models where we use ordered categorical outcomes 
# (Likert, i.e., ordlogit). In this file we do the logistic regressions.
my_data <- data.frame(
  experience_s = data_long$experience_s,
  # Set ET_TRAD == 1 else 2
  treatment = as.integer(ifelse( data_long$treatment=="ET_TRAD" , 1 , 2 )),
  measure = as.integer(data_long$measurement)
)

# remove two NAs
my_data <- my_data[complete.cases(my_data), ]

# A model where we try to predict the outcome "measure" using treatment 
# (ET_TRAD or ET_BA) and experience in years, as predictors. Below you will find
# a PSIS-LOO comparisons of the three basic models (_2 with only intercept, and 
# _3 with varying intercept for the two treatments). As is evident, we can use 
# experience as a predictor.
m_ET_TRAD_vs_BA <- map2stan(
  alist(
    measure ~ dbinom(1, p),
    logit(p) <- a + b[treatment] + b_EXP * experience_s,
    c(a, b_EXP) ~ dnorm(0, 1.5),
    b[treatment] ~ dnorm(0, 0.5)
  ), data = my_data, chains = 4, cores = 4, iter=1e4
)

#> compare(m_ET_TRAD_vs_BA, m_ET_vs_BA_2, m_ET_vs_BA_3, func=LOO)
#                    LOO pLOO dLOO weight   SE  dSE
# m_ET_vs_BA_3      39.8  1.2  0.0   0.60 6.91   NA
# m_ET_TRAD_vs_BA   41.4  2.1  1.5   0.28 7.78 2.14
# m_ET_vs_BA_2      43.1  0.9  3.3   0.12 7.59 1.94
# 
# In short, for the 95% interval (z-score = 1.96) of the diff
# > 1.5 + c(-1,1)*2.14*1.96
# [1] -2.6944  5.6944
# So it really doesn't matter if we pick the first- or the second-ranked model
#
# m_ET_vs_BA_2 <- ulam(
#   alist(
#     measure ~ dbinom(1, p),
#     logit(p) <- a,
#     a ~ dnorm(0, 1.5)
#   ), data = my_data, chains = 4, cores = 4, log_lik = TRUE
# )
# 
# m_ET_vs_BA_3 <- ulam(
#   alist(
#     measure ~ dbinom(1, p),
#     logit(p) <- a + b[treatment],
#     c(a, b_EXP) ~ dnorm(0, 1.5),
#     b[treatment] ~ dnorm(0, 0.5)
#   ), data = my_data, chains = 4, cores = 4, log_lik = TRUE
# )
################################################################################
# Next, let's design a model for the comparison of the answers concerning using
# more (ME) or less (LE) experienced testers.

# Make the data long format and overwrite previous var
data_long <- gather(d, treatment, measurement, ME_TRAD, ME_BA)

my_data <- data.frame(
  experience_s = data_long$experience_s,
  # Set ME_TRAD == 1 else 2
  treatment = as.integer(ifelse(data_long$treatment=="ME_TRAD" , 1 , 2 )),
  measure = as.integer(data_long$measurement)
)

# remove one NA
my_data <- my_data[complete.cases(my_data), ]

m_ME_TRAD_vs_BA <- map2stan(
  alist(
    measure ~ dbinom(1, p),
    logit(p) <- a + b[treatment] + b_EXP * experience_s,
    c(a, b_EXP) ~ dnorm(0, 1.5),
    b[treatment] ~ dnorm(0, 0.5)
  ), data = my_data, chains = 4, cores = 4, iter=1e4
)

################################################################################
# Sensitivity analysis of priors
# Let's assume an intercept \alpha, varying intercepts for our two treatments
# (TRAD and BA), and a parameter \beta for estimating EXPERIENCE.

# Let's assume broad priors, i.e., dnorm(0, 10) and dnorm(0, 5), respectively.
m_priors <- map2stan(
  alist(
    measure ~ dbinom(1, p),
    logit(p) <- a + b[treatment] + b_EXP * experience_s,
    c(a, b_EXP) ~ dnorm(0, 10),
    b[treatment] ~ dnorm(0, 5)
  ), data = my_data, chains = 4, cores = 4, iter=1e4
)

prior <- extract.prior(m_priors)

p <- sapply( 1:2 , function(k) inv_logit(prior$a + prior$b[,k] + prior$b_EXP))
plot(density(p, adjust=0.3), col="blue", xlab="Outcome", main="", bty="n")

# So extreme values 0 or 1 are expected.

# In the model we designed previously (m_ET_TRAD_vs_BA) we assumed:
# \alpha ~ N(0, 1.5)
# b[treatment] ~ N(0, 0.5)
# b_EXP ~ N(0, 1.5)
# which seems to be very tight priors, but let's have a look what it means
prior <- extract.prior(m_ET_TRAD_vs_BA)

p <- sapply( 1:2 , function(k) inv_logit(prior$a + prior$b[,k] + prior$b_EXP))
lines(density(p, adjust=0.3))
# Much more modest and even though we have spikes at the start and end (which we 
# should have), the priors also assumes values in-between.

################################################################################
# We now have two models which we've sampled using Stan and we've looked into
# the sensitivity of our priors.
# Diagnostics looks ok (n_eff, \hat{R}, pairs plots, trace plots)
# pairs(m_ET_TRAD_vs_BA)
# tracerplot(m_ET_TRAD_vs_BA)

# Let's see what we've found.
par(bty="n")
plot(precis(m_ET_TRAD_vs_BA, depth=2), xlab="", main="")
#        mean   sd  5.5% 94.5% n_eff Rhat
# a      1.88 0.55  1.02  2.78 13849    1
# b_EXP -0.19 0.46 -0.91  0.54 19332    1
# b[1]   0.03 0.45 -0.68  0.74 15409    1
# b[2]   0.19 0.44 -0.52  0.90 15846    1
#
# Not really much of a difference here when discussing if we should introduce ET
# So the mean (a) is clearly positive, i.e., they lean strongly towards `Yes' 
# concerning the question: 
# Based on the above information, would you introduce exploratory 
# testing techniques in your context?
#
# However, there's a difference between b[1] (TRAD) and b[2] (BA). The posterior 
# for TRAD, b[1], is lower than that of BA, b[2]. 
# How much lower? 
# We need to compute the contrast. Let’s calculate the contrast on the logit 
# scale (relative effect), as well as on the outcome scale (absolute effect).

# Extract samples from the posterior
post_ET <- extract.samples(m_ET_TRAD_vs_BA)

# logit scale, i.e., relative effect
diff_a <- post_ET$b[,1] - post_ET$b[,2] 

# outcome scale, i.e., absolute effect
diff_p <- inv_logit(post_ET$b[,1]) - inv_logit(post_ET$b[,2]) 

precis(list(diff_a=diff_a, diff_p=diff_p))
#'data.frame': 20000 obs. of 2 variables:
#         mean   sd  5.5% 94.5%   histogram
# diff_a -0.16 0.56 -1.06  0.75   ▁▁▁▃▇▅▂▁▁
# diff_p -0.04 0.13 -0.25  0.18 ▁▁▁▂▅▇▇▂▁▁▁
#
# The log-odds difference is certainly negative (-0.16), corresponding to a 
# lower prob of answering `Yes' when TRAD. However, on the probability scale 
# (absolute effect), the difference is somewhere between -25% and 18% (using 
# 89% HPDI). Not much to talk about here, let's move along... But it shows the 
# importance of looking at relative and absolute effects!

# Let's look at the next model, i.e., more experienced (ME) testers.
plot(precis(m_ME_TRAD_vs_BA, depth=2), bty="n", xlab="")
#        mean   sd  5.5% 94.5% n_eff Rhat
# a      1.46 0.51  0.65  2.29 11214    1
# b_EXP  0.29 0.41 -0.35  0.96 17321    1
# b[1]  -0.35 0.43 -1.04  0.34 12150    1
# b[2]   0.52 0.44 -0.18  1.22 13786    1
#
# So there's seems to be a larger effect (look at the contrast between TRAD, 
# b[1], and BA, b[2].

post_ME <- extract.samples(m_ME_TRAD_vs_BA)
diff_a <- post_ME$b[,1] - post_ME$b[,2]
diff_p <- inv_logit(post_ME$b[,1]) - inv_logit(post_ME$b[,2])
precis(list(diff_a=diff_a, diff_p=diff_p))
#'data.frame': 20000 obs. of 2 variables:
#         mean   sd  5.5% 94.5%   histogram
# diff_a -0.87 0.53 -1.73 -0.03  ▁▁▁▂▅▇▃▁▁▁
# diff_p -0.20 0.12 -0.40 -0.01 ▁▁▁▃▇▇▃▁▁▁▁

# The log-odds difference -0.87 is certainly negative, corresponding to a
# lower probability for TRAD answering `Yes' to the question:
# Based on the above information, would you utilize more senior testers?
# On the probability scale itself, the difference is somewhere between -40% 
# and -1% for 89% HPDI.