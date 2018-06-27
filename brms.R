# MAINTAINER Richard Torkar richard.torkar@gmail.com
# Version 1.0
#
# Run this in RStudio to have plots automatically being taken care of:
# https://www.rstudio.com
# If you are lazy, who isn't(?) just run this if you have Docker installed:
# docker run -d -p 8787:8787 -v "`pwd`":/home/rstudio/working -e PASSWORD=rstudio -e ROOT=TRUE torkar/docker-b3
# then fire up your browser and enter localhost:8787 and the username and 
# password: rstudio

library(brms) # rethinking & rstanarm are other packages one can use instead
library(bayesplot) # *the* package for plotting BDA output by Gabry et al.
library(sjstats) # for ROPE analysis
library(gridExtra) # Multiple plots in one plot
library(pt) # prospect theory package
library(plyr) # 
library(ggthemes) # We want simple clean plots
ggplot2::theme_set(theme_tufte())
bayesplot::color_scheme_set("darkgray")

# For reproducibility.
SEED <- 61215 # My oldest boy was born 2006, Dec. 15 :)
set.seed(SEED)

# Set number of cores
options(mc.cores = parallel::detectCores())

# Set the location for dataFile!
# dataFile <- "~/Documents/cth/Research Projects, Studies & Data/Bayesian/Study III/data.csv"
dataFile <- "/home/rstudio/data.csv"
d <- read.csv2(dataFile, sep=";")

# In total 70 observations (each subject tried both techniques and the exp 
# design ensured that we avoided learning bias):
#
# category - more experienced (ME/1) or less experienced (LE/2)
# technique - old technique (OT/2) or new technique (NT/1)
# subject - 1:35
# tp - true positive 0:25 -> the higher the better
#
# Show summary of the dataset
str(d)
#'data.frame':	70 obs. of  4 variables:
# $ subject  : int  1 1 2 2 3 3 4 4 5 5 ...
# $ category : Factor w/ 2 levels "LE","ME": 1 1 1 1 1 1 1 1 1 1 ...
# $ technique: Factor w/ 2 levels "NT","OT": 1 2 1 2 1 2 1 2 1 2 ...
# $ tp       : int  20 1 4 1 9 0 4 2 9 1 ...

# Let's plot some things so we get a feeling for how it looks like
# First plot the density of each technique
d$category_num <- as.numeric(d$category)
d$technique_num <- as.numeric(d$technique)

oldTech <- d[which(d$technique_num == 1),]
newTech <- d[which(d$technique_num == 2),]
oldTech <- density(oldTech$tp)
newTech <- density(newTech$tp)
plot(newTech, ylim=c(0,0.3), xlim=c(-2,25), 
     xlab="Number of true positives (tp)", main="", bty="n", lty=2)
lines(oldTech)
text(x = 5, y = 0.2, "Old technique (OT)")
text(x = 11, y = 0.1, "New technique (NT)")
# So in absolute measures NT seems better?

# Plot the density of each category of subjects
catExp <- d[which(d$category_num == 1),]
catNonExp <- d[which(d$category_num == 2),]
catNonExp <- density(catNonExp$tp)
catExp <- density(catExp$tp)
plot(catExp, ylim=c(0,0.12), xlim=c(-3,25), xlab="Number of true positives (tp)", 
     main="", bty="n", lty=2)
lines(catNonExp)
text(x = 7, y = 0.1, "Less experienced (LE)")
text(x = 3, y = 0.044, "More experienced (ME)")
# So, not a much difference perhaps?
################################################################################
#
# Now let us create a model to test the sanity of our assumptions. Note the use 
# of sample_prior="only", i.e., we are ignoring the likelihood at the moment.
m0.1 <- brm(formula = tp ~ technique + category + subject,
              data = d,
              prior = c(set_prior("normal(0,0.1)", class="Intercept"),
                        set_prior("normal(0,0.1)", class="b")),
              family = poisson(link=log),
              seed = SEED,
              refresh = 0,
              sample_prior = "only"
)

# If we only sample from the priors, using N(0,1), and include our four 
# additive terms, then var, since it's log intensity, is equal to (1*4)^2 = 16.
# Hence, we should have N(0,<<1).
#
# If we empirically check our hypothesis reg. priors by sampling n=100 from each 
# posterior then max x when N(0, c(1, 0.1, 0.01)) is ~2*10^9 (with 30+ cases 
# of overflow), ~60--200, ~5--8, respectively. 80--200 is still much higher 
# than the max value we expect, i.e., 25, so if we use N(0,0.1) we still leave 
# ample room for extreme values:

m1 <-
  update(m0.1, prior = c(
    set_prior("normal(0,1)", class = "Intercept"),
    set_prior("normal(0,1)", class = "b")),
    sample_prior = "only",
    refresh = 0,
    seed = SEED
    )

m0.01 <-
  update(m0.1, prior = c(
    set_prior("normal(0,0.01)", class = "Intercept"),
    set_prior("normal(0,0.01)", class = "b")),
    sample_prior = "only",
    refresh = 0,
    seed = SEED
    )

# Get 100 samples from the posteriors of each model
s1 <- posterior_predict(m1, nsamples = 100)
s0.1 <- posterior_predict(m0.1, nsamples = 100)
s0.01 <- posterior_predict(m0.01, nsamples = 100)

#Check what the max values are
max(s1, na.rm = T)
# [1] 2122506522
max(s0.1, na.rm = T)
# [1] 42

max(s0.01, na.rm = T)
# [1] 8

# So in short, we will be just fine using N(0,0.1), but as we will see Stan 
# handles wider priors w/o a problem in this case. If we would have problems
# with divergence and chains that do not mix well we need to look into this 
# further.
################################################################################
#
# Set upper bound to 25 for the outcome (there were a total of 
# 25 faults) and, for the last three models below, we try to predict the 
# zero-inflation probability depending on the technique used since we have 18.6% 
# zeros in our response variable. Also, up the priors since trunc affects the 
# span of valid respones, i.e., 0:25. The same applies when using zi 
# distributions, unfortunately...

# Number of zeros in our outcome variable:
prop_zero <- function(x) mean(x == 0)
prop_zero(d$tp) 

m2 <- brm(formula = tp | trunc(ub=25) ~ technique + category + subject,
          data = d,
          prior = c(set_prior("normal(0,1)", class="b")),
          family = poisson(link=log),
          refresh = 0,
          seed=SEED
)

# Using a zero-inflated Poission distribution means we need to remove truncation 
# from the below model.
m2_zi_t <- brm(bf(formula = tp ~ technique + category + 
                        subject, zi ~ technique),
               data = d,
               prior = c(set_prior("normal(0,1)", class="b")),
               family = zero_inflated_poisson(link=log),
               refresh = 0,
               seed=SEED
)

# In the end, comparing these things visually is hard, so we can use state of 
# art model comparison, i.e., PSIS-LOO, Pareto-Smoothed Importance Samling - 
# Leave One Out cross sampling. 
# https://arxiv.org/pdf/1507.04544.pdf
# The LOOIC values should be as low as possible. 
loo::loo(m2, m2_zi_t)
#               LOOIC    SE
# m2           326.24 21.89
# m2_zi_t      323.00 22.07
# m2 - m2_zi_t   3.24  5.50

# So, the above indicates that using a zero-inflated Poisson (when 
# zi depends on technique) has better prediction accuracy (lower LOOIC values),
# but the SE diff is 5.50 while the absolute diff is ~3 so not exactly 
# crystal clear.
################################################################################
#
# Now, let us assume that the intercept varies across subjects (1 | subject), 
# i.e., group-level effect, but the effect of technique and category to be 
# constant across subjects (population-level effect). In short, technique and 
# category effects are "fixed" because no matter where, how, or how many 
# subjects we'd have sampled, we would still have the same levels in the same 
# variables: Technique 1 and 2, and Category 1 and 2.
m3_vi <- brm(bf(formula = tp ~ 1 + technique + category +
                      (1 | subject), zi ~ technique),
             data = d,
             prior = c(set_prior("normal(0,10)", class="Intercept"),
                       set_prior("normal(0,1)", class="Intercept", 
                                 dpar="zi"),
                       set_prior("normal(0,1)", class="b"), 
                       set_prior("cauchy(0,1)", class="sd")),
             family = zero_inflated_poisson(link=log),
             chains = 4,
             iter = 10000,
             seed = SEED,
             refresh = 0,
             sample_prior = "yes"
)

# Same as previous model, but here we also take into account varying slopes
m4_vi_vs <- brm(bf(formula = tp ~ 1 + technique + category + 
                         (1 + technique | subject), zi ~ technique),
                data = d,
                prior = c(set_prior("normal(0,10)", class="Intercept"),
                          set_prior("normal(0,1)", class="Intercept", 
                                    dpar="zi"),
                          set_prior("normal(0,1)", class="b"), 
                          set_prior("cauchy(0,1)", class="sd")),
                family = zero_inflated_poisson(link=log),
                control = list(adapt_delta=0.999),
                refresh = 0,
                seed = SEED
)

# Let's compare the models we've designed:
# m2: We add trunc and set N(0,1).
# m2_zi_t: The zi Poisson model where the zi is depending on technique.
# m3_vi: as previous + varying intercepts.
# m4_vi_vs: as previous + varying slopes.
#
# We run with reloo=TRUE for exact leave-one-out cross-validation.
loo::loo(m2, m2_zi_t, m3_vi, m4_vi_vs, reloo=T)
# The above should end up in something like this:
#                     LOOIC    SE
# m2                 326.24 21.89
# m2_zi_t            323.00 22.07
# m3_vi              317.90 18.60
# m4_vi_vs           316.63 17.73
# m2 - m2_zi_t         3.24  5.50
# m2 - m3_vi           8.34  8.79
# m2 - m4_vi_vs        9.61  8.81
# m2_zi_t - m3_vi      5.09  6.39
# m2_zi_t - m4_vi_vs   6.37  6.61
# m3_vi - m4_vi_vs     1.27  2.38
#
# As we see in the table fit_vi_vs, i.e. our varying intercept *and* varying 
# slopes model has the best out of sample prediction. If we compare that model 
# with fit_vi, i.e., our varying intercept model, we see that the absolute 
# difference is 1.27 and that dSE=2.38. The question is if we should pick that 
# model? A varying slopes and varying intercepts model is very complex and some 
# would argue that that complexity literally doesn't add anything, or as 
# R. McElreath would say "This is a model only a mother would love."
#
# In this case, we opt for the simpler model, for reasons of understandability 
# and usefulness.
################################################################################
#
#
# Let's start by renaming our model now that we've decided that this is the one
# we should keep. Let us call it the model with a capital M ;)

M <- m3_vi
tidy_stan(M, prob=.95, type="all")
# Let's see a summary of the statistics the model has estimated. Here we 
# print out all estimates so that we even get an estimate for each subject.
## Conditional Model: Fixed effects
# 
#             estimate std.error      HDI(95%) neff_ratio Rhat mcse
# Intercept       1.96      0.10 [ 1.76  2.14]          1    1    0
# techniqueOT    -1.42      0.17 [-1.77 -1.09]          1    1    0
# categoryME      0.32      0.15 [ 0.01  0.62]          1    1    0
# 
# ## Conditional Model: Random effect (Intercept)
# 
#             estimate std.error      HDI(95%) neff_ratio Rhat mcse
# subject.1      0.47      0.25 [-0.01  0.93]          1    1    0
# subject.2     -0.17      0.23 [-0.66  0.26]          1    1    0
# subject.3      0.05      0.22 [-0.38  0.53]          1    1    0
# subject.4     -0.12      0.21 [-0.60  0.29]          1    1    0
# subject.5      0.04      0.20 [-0.38  0.47]          1    1    0
# subject.6     -0.03      0.21 [-0.49  0.39]          1    1    0
# subject.7      0.24      0.22 [-0.16  0.70]          1    1    0
# subject.8      0.10      0.22 [-0.32  0.58]          1    1    0
# subject.9     -0.07      0.22 [-0.56  0.37]          1    1    0
# subject.10    -0.08      0.21 [-0.55  0.36]          1    1    0
# subject.11    -0.17      0.23 [-0.67  0.27]          1    1    0
# subject.12    -0.08      0.21 [-0.58  0.35]          1    1    0
# subject.13    -0.08      0.21 [-0.53  0.35]          1    1    0
# subject.14     0.13      0.20 [-0.27  0.56]          1    1    0
# subject.15     0.23      0.23 [-0.19  0.71]          1    1    0
# subject.16    -0.12      0.22 [-0.61  0.30]          1    1    0
# subject.17    -0.17      0.23 [-0.67  0.26]          1    1    0
# subject.18    -0.17      0.24 [-0.70  0.27]          1    1    0
# subject.19     0.08      0.20 [-0.32  0.53]          1    1    0
# subject.20    -0.07      0.22 [-0.57  0.35]          1    1    0
# subject.21     0.06      0.21 [-0.38  0.51]          1    1    0
# subject.22     0.12      0.21 [-0.28  0.58]          1    1    0
# subject.23    -0.08      0.21 [-0.55  0.34]          1    1    0
# subject.24     0.02      0.19 [-0.37  0.46]          1    1    0
# subject.25    -0.23      0.24 [-0.75  0.20]          1    1    0
# subject.26     0.06      0.19 [-0.34  0.48]          1    1    0
# subject.27     0.33      0.22 [-0.07  0.77]          1    1    0
# subject.28     0.03      0.20 [-0.39  0.45]          1    1    0
# subject.29    -0.09      0.21 [-0.59  0.32]          1    1    0
# subject.30    -0.04      0.20 [-0.47  0.37]          1    1    0
# subject.31    -0.18      0.23 [-0.68  0.25]          1    1    0
# subject.32    -0.20      0.22 [-0.68  0.21]          1    1    0
# subject.33    -0.01      0.20 [-0.41  0.41]          1    1    0
# subject.34     0.23      0.21 [-0.16  0.66]          1    1    0
# subject.35     0.14      0.21 [-0.26  0.60]          1    1    0
# 
# ## Zero-Inflated Model: Fixed effects
# 
#             estimate std.error      HDI(95%) neff_ratio Rhat mcse
# Intercept      -3.62      0.92 [-5.67 -2.03]          1    1 0.01
# techniqueOT     2.70      1.11 [ 0.57  5.12]          1    1 0.01

# Everything looks ok, but what sticks out is that \beta_c, represented here by
# the variable categoryME, seems to be significant. The 95% Highest Density 
# Interval (HDI) seems to *not* cross zero.
#
# If we would have Rhat values of 1.01 we would standardize the predictors 
# technique and category, i.e., (x - mean(x)) / sd(x)
# d$category_s <- (d$category_num - mean(d$category_num))/sd(d$category_num)
# d$technique_s <- (d$technique_num - mean(d$technique_num))/sd(d$category_num)
# and then run the sampling procedure again.
################################################################################
#
# Check some diagnostics
#
# Create trace plots to see that we have well-mixed chains, and density plots
# of the parameters of interest.
posterior <- as.array(M)

# Check the names we have in our array
dimnames(posterior)
# $iterations
# NULL
# 
# $chains
# [1] "chain:1" "chain:2" "chain:3" "chain:4"
# 
# $parameters
# [1] "b_Intercept"             "b_zi_Intercept"          "b_techniqueOT"           "b_categoryME"           
# [5] "b_zi_techniqueOT"        "sd_subject__Intercept"   "r_subject[1,Intercept]"  "r_subject[2,Intercept]" 
# [9] "r_subject[3,Intercept]"  "r_subject[4,Intercept]"  "r_subject[5,Intercept]"  "r_subject[6,Intercept]" 
# [13] "r_subject[7,Intercept]"  "r_subject[8,Intercept]"  "r_subject[9,Intercept]"  "r_subject[10,Intercept]"
# [17] "r_subject[11,Intercept]" "r_subject[12,Intercept]" "r_subject[13,Intercept]" "r_subject[14,Intercept]"
# [21] "r_subject[15,Intercept]" "r_subject[16,Intercept]" "r_subject[17,Intercept]" "r_subject[18,Intercept]"
# [25] "r_subject[19,Intercept]" "r_subject[20,Intercept]" "r_subject[21,Intercept]" "r_subject[22,Intercept]"
# [29] "r_subject[23,Intercept]" "r_subject[24,Intercept]" "r_subject[25,Intercept]" "r_subject[26,Intercept]"
# [33] "r_subject[27,Intercept]" "r_subject[28,Intercept]" "r_subject[29,Intercept]" "r_subject[30,Intercept]"
# [37] "r_subject[31,Intercept]" "r_subject[32,Intercept]" "r_subject[33,Intercept]" "r_subject[34,Intercept]"
# [41] "r_subject[35,Intercept]" "lp__"

# Create trace plots, one for each parameter of interest.
p_intercept <- mcmc_trace(posterior, pars = c("b_Intercept")) +
  theme(legend.position="none", 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  labs(y=expression(alpha))

p_tech <- mcmc_trace(posterior, pars = c("b_techniqueOT")) +
  theme(legend.position="none", 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  labs(y=expression(beta[t]))

p_cat <- mcmc_trace(posterior, pars = c("b_categoryME")) +
  theme(legend.position="none", 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  labs(y=expression(beta[c]))

p_sd <- mcmc_trace(posterior, pars = c("sd_subject__Intercept")) +
  theme(legend.position="none", 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  labs(y=expression(paste("subject ", alpha[sigma])))

p_zi_i <- mcmc_trace(posterior, pars = c("b_zi_Intercept")) +
  theme(legend.position="none", 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  labs(y=expression(paste("zi ", alpha)))

p_zi_t <- mcmc_trace(posterior, pars = c("b_zi_techniqueOT")) +
  theme(legend.position="none", 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  labs(y=expression(paste("zi ", beta["t"])))

# Arrange the plots in 2 rows
grid.arrange(p_intercept, p_tech, p_cat, p_zi_i, p_zi_t, p_sd, nrow=2)
# We see well-mixed chains, as should be the case.

# Let's finally examine neff (should be >0.1)
ratios_neff <- neff_ratio(M)
mcmc_neff(ratios_neff)

# and Rhat values also
max(rhat(M))
# Should be ~1.001 so well below 1.1 :)

# Plot y vs y_rep density
ppc_dens_overlay(y=d$tp, 
                 yrep=posterior_predict(M, nsamples=100)) + 
  theme(legend.position="none",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) 

# All of the above diagnostics look ok.
################################################################################
#
# Compare how agressive the partial pooling is with a complete pooling model
m_cop <- brm(bf(formula = tp ~ 1),
             data = d,
             family = poisson(link=log),
             seed=SEED
)

# Plot the intervals (first 11 subjects) and compare the difference
ppc_m <- pp_check(M, type="intervals", prob=0.95) + 
  coord_cartesian(xlim=c(1,11), ylim=c(0,20)) + 
  ggtitle("Partial pooling") + 
  xlab("") + ylab("") + 
  theme(legend.position="none",
        plot.title = element_text(hjust=1),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.y = element_line(colour="gray", size=0.1)) +
  geom_abline(intercept=0,slope=0)

ppc_cop <- pp_check(m_cop, type="intervals", prob=0.95) +
  coord_cartesian(xlim=c(1,11), ylim=c(0,20)) +
  ggtitle("Complete pooling") +
  xlab("") +
  ylab("") +
  theme(legend.position="none",
        plot.title = element_text(hjust=1),
        panel.grid.major.y = element_line(colour="gray", size=0.1)) +
  geom_abline(intercept=0,slope=0)

grid.arrange(ppc_m, ppc_cop, nrow=2)
################################################################################
# Make more sanity checks of our model, i.e. 
# Look at density plots of params
# Check pairs plot
# Check marginal effects
# Plot fitted means against actual response (tp)
# Check >1 density plots
# Check proportion of zeros

# Two density plots of beta_t and beta_c 
mcmc_areas(posterior, pars = c("b_categoryME","b_techniqueOT"), prob = 0.95) + 
  theme(axis.ticks.y=element_blank(),
        axis.text.y=element_blank())

#####
# Pairs plot. Set titles using math notation
mcmc_p <- mcmc_pairs(posterior, 
                     pars = c("b_Intercept",  "b_techniqueOT", "b_categoryME"),
                     off_diag_args = list(size = 0.5),
                     diag_fun = "dens")

# split plot into subplots
plots <- mcmc_p$bayesplots

# add math stats notation to the plots
plots[[1]] <- plots[[1]] + labs(subtitle=expression(italic(alpha)))
plots[[5]] <- plots[[5]] + labs(subtitle=expression(italic(beta[t])))
plots[[9]] <- plots[[9]] + labs(subtitle=expression(italic(beta[c])))

# Plot the new plot in RStudio. Check that it is a 3x3 plot (RStudio seems to 
# be a bit shaky so sometimes it only plots the first 1...3 plots)
bayesplot_grid(plots = plots)
################################################################################

# If we check the margeff we see instantaneous changes in the outcome depending 
# on the predictors.
plot(marginal_effects(M), ask=FALSE)

# Plot fitted means against actual response (see ?fitted.brmsfit)
dat <- as.data.frame(cbind(Y=standata(M)$Y, fitted(M)))
ggplot(dat)  + geom_point(aes(x = Estimate, y = Y)) +
  xlab(expression(italic(y)[rep])) +
  ylab(expression(italic(y))) + 
  ylim(0,20) + 
  xlim (0,15)

# Density plots of y and 5 y_rep densities, sampled w/ 1000 draws each
yrep<-posterior_predict(M, draws=1000)
y <- d$tp
ppc_dens(y, yrep[1:5,])

# Check proportion of 0 using test statistics
ppc_stat(y, yrep, stat="prop_zero") + theme(legend.position = "none")

################################################################################
# One of our params seems to be significant, i.e., 95% CI is not crossing zero.
# But to include \beta_c in out further analysis we want to be sure that it has
# a decent effect. One way to look at the size of the effect is to conduct a 
# ROPE analysis (Region of Practical Equivalence). Below we check if the effect
# is significant as a "small effect", i.e., equivalent to Cohen's d = 0.2.
#
# ROPE analysis on +/- 0.1
equi_test(M, eff_size = 0.1)
#                      H0 %inROPE      HDI(95%)
# b_Intercept      reject    0.00 [ 1.76  2.14]
# b_zi_Intercept   reject    0.00 [-5.67 -2.03]
# b_techniqueOT    reject    0.00 [-1.77 -1.09]
# b_categoryME     reject    0.57 [ 0.01  0.62]
# b_zi_techniqueOT reject    0.04 [ 0.57  5.12]
#
# This is an indication of \beta_c not being as relevant as we thought. 

################################################################################
#
# One of the benefits of having a posterior distribution is that it's very easy
# to simulate new outcomes. Below we simulate 35 new subjects in addition to the 
# 35 we already have.
os <- exp(ranef(M)$subject[,1,] + fixef(M)[1,1])
os <- as.data.frame(os)
os$Subject<-rownames(os)
colnames(os)[colnames(os)=="os"] <- "Intercept"
os <- os[,c(2,1)]

# This line creates a dataframe for 35 hypothetical new subjects by using the 
# estimated standard deviation.
new_subject <- data.frame(Subject = as.character(36:70),
                          Intercept= exp(fixef(M)["Intercept", "Estimate"]) + 
                            rnorm(35, 0, exp(summary(M)$random$subject[,"Estimate"])),
                          Status="Simulated")
os$Status <- "Observed"
os <- rbind(os,new_subject)
os$Subject <- as.numeric(os$Subject)

ggplot(os,aes(x=Subject, y=Intercept, shape=Status)) +
  geom_point() +
  geom_hline(aes(yintercept = exp(fixef(M)["Intercept", "Estimate"]))) +
  geom_vline(xintercept=35.5, size=0.05) +
  ylab("tp") +
  theme(legend.position="none")

################################################################################
# Let's do some simulations
#

# Set the different experience levels we have, and then NT or OT.
newdata_nt <- data.frame(category=c("LE","ME"), technique=c("NT"))
newdata_ot <- data.frame(category=c("LE","ME"), technique=c("OT"))

# Predict responses based on the fitted model.
# Here we incorporate pred errors, hence the variance will be higher!
# First we make predictions for less (LE) and more experienced (ME) subjects 
# using the new technique (NT)
p_nt <- posterior_predict(M, newdata = newdata_nt, re_formula = NA)
hdi_nt_le <- hdi(p_nt[,1], prob=.94)
med_nt_le <- median(p_nt[,1])
hdi_nt_me <- hdi(p_nt[,2], prob=.94)
med_nt_me <- median(p_nt[,2])

# Next we do the same for the old technique (OT)
p_ot <- posterior_predict(M, newdata = newdata_ot, re_formula = NA)
hdi_ot_le <- hdi(p_ot[,1], prob=.94)
med_ot_le <- median(p_ot[,1])
hdi_ot_me <- hdi(p_ot[,2], prob=.94)
med_ot_me <- median(p_ot[,2])

# Calc total NT median and HDI
foo <- p_nt[,1]
bar <- p_nt[,2]
foo <- append(foo,bar)
hdi_tutti_nt <- hdi(foo, prob=0.94)
med_tutti_nt <- median(foo)

# Calc total OT median and HDI
foo <- p_ot[,1]
bar <- p_ot[,2]
foo <- append(foo,bar)
hdi_tutti_ot <- hdi(foo, prob=0.94)
med_tutti_ot <- median(foo)

# Calc total for all subjects no matter experience or technique

################################################################################
# Let us now connect this to prospect theory
#
#
# First we plot the s-curve in prospect theory

# Plot prospect theory curve using alpha=0.61 as in the original paper by 
# Kahneman & Tverksy (1992)
plotOneParProbWFam(my_x_label = "p", my_y_label = "w(p)", pwf=kt_pwf,
                   par=c(0.61),
                   draw_reference_line_flag=TRUE, reference_line_colour="black",
                   reference_line_style="dotted",
                   font_scaling=1.0
)

# For both games, in total we have 6 choices,
choice_ids <- c(1,1,1,1,1,1)

# the choices are divided equally into two branches,
gamble_ids <- c(1,1,1,2,2,2)

# and we have three distinct outcomes in each branch,
outcome_ids <- c(1,2,3,1,2,3)

# For the two games (experience and technique) we have the following values:
# (This is the indata; change if needed)
# Salary: 
salary_le <- 100 # $100/h 
salary_me <- 200 # $200/h

# Num hours of work in a session: 4h
session_time <- 4
cost_session_me <- session_time * salary_me
cost_session_le <- session_time * salary_le

# And average salary for the experiment
cost_session_avg <- ((23 * salary_le) + (12 * salary_me) )/35

# In addition, we have one more thing we need to take into account:
# How much does a bug cost if not found before release?
# According to Boehm & Basili (2001) they suggest a ratio 1:5 for small, 
# noncritical systems, while for critical systems it can be 1:100. Of course,
# we have some massive outliers here...
# We've seen companies estimate the cost of a released bug to be anything 
# between $150 and $3,000, so let us aim low here since it was a small, 
# noncritical system (you can change as you want)
cost_for_bug <- 150

# Two vectors to store the $ for each scenario e and t.
cons_e <- c()
cons_t <- c()

# For every HDI either -1 or +1 depending on if it's lower or upper bound
# Then multiply each bug with the cost for the bug - the cost for the session, 
# which differs depending on LE and ME, or if a mix of the two.
cons_e <- c(round_any((hdi_nt_me[1]-1) * cost_for_bug - cost_session_me, 10))
cons_e <- c(cons_e, round_any(med_nt_me * cost_for_bug - cost_session_me,10))
cons_e <- c(cons_e, round_any((hdi_nt_me[2]+1) * cost_for_bug - cost_session_me,10))
cons_e <- c(cons_e, round_any((hdi_nt_le[1]-1) * cost_for_bug - cost_session_le,10))
cons_e  <- c(cons_e, round_any(med_nt_le * cost_for_bug - cost_session_le, 10))
cons_e <- c(cons_e, round_any((hdi_nt_le[2]+1) * cost_for_bug - cost_session_le,10))

cons_t <- c(round_any(hdi_tutti_nt[1] * cost_for_bug - cost_session_avg,10))
cons_t <- c(cons_t, round_any(med_tutti_nt * cost_for_bug - cost_session_avg,10))
cons_t <- c(cons_t, round_any((hdi_tutti_nt[2] + 1) * cost_for_bug - cost_session_avg,10))
cons_t <- c(cons_t, round_any(hdi_tutti_ot[1] * cost_for_bug - cost_session_avg,10))
cons_t <- c(cons_t, round_any(med_tutti_ot * cost_for_bug - cost_session_avg,10))
cons_t <- c(cons_t, round_any((hdi_tutti_ot[2] + 1) * cost_for_bug - cost_session_avg,10))

# The probabilties are the same in both games so we can reuse this for both.
# If you change the probs in the previous section you need to obviously change 
# it here.
probability_strings <- c("0.03", "0.94", "0.03", 
                         "0.03", "0.94", "0.03")

# Let's put everything together in two Choices objects my_choicesA and 
# my_choicesB
choices_epsilon <- Choices(choice_ids=choice_ids,
                      gamble_ids=gamble_ids,
                      outcome_ids=outcome_ids,
                      objective_consequences=cons_e,
                      probability_strings=probability_strings)

choices_tau <- Choices(choice_ids=choice_ids,
                       gamble_ids=gamble_ids,
                       outcome_ids=outcome_ids,
                       objective_consequences=cons_t,
                       probability_strings=probability_strings)

# Plot trees of the two games
# We call this tree \epsilon since we look at *E*xperience
drawChoices(choices_epsilon,
            decision_square_x=0.2, decision_square_edge_length=0.05,
            circle_radius=0.025, y_split_gap=0.1, x_split_offset=0.03,
            probability_text_digits=3, y_probability_text_offset=0.015,
            y_value_text_offset=0.005, x_value_text_offset=0.025,
            probability_text_font_colour="black", probability_text_font_size=11,
            objective_consequence_text_font_colour="black",
            objective_consequence_text_font_size=11, 
            label=c("A","B", expression(epsilon)),
            label_font_colour=c("black","black", "black"),
            label_font_size=c(11,11,24),
            label_positions=list(c(0.305,0.65),c(0.305,0.35),c(0.2,0.5)))

# We call this tree \tau since we look at the *t*echniques
drawChoices(choices_tau,
            decision_square_x=0.2, decision_square_edge_length=0.05,
            circle_radius=0.025, y_split_gap=0.1, x_split_offset=0.03,
            probability_text_digits=3, y_probability_text_offset=0.015,
            y_value_text_offset=0.005, x_value_text_offset=0.025,
            probability_text_font_colour="black", probability_text_font_size=11,
            objective_consequence_text_font_colour="black",
            objective_consequence_text_font_size=11, 
            label=c("A","B",expression(tau)),
            label_font_colour=c("black","black", "black"),
            label_font_size=c(11,11,24),
            label_positions=list(c(0.305,0.65),c(0.305,0.35), c(0.2,0.5)))

################################################################################
# Let's calculate the PTU

# Use utility settings according to Kahneman & Tversky (1992)
tk_1992_utility <- Utility(fun="power", 
                           par=c(alpha=0.88, beta=0.88, lambda=2.25))

# Use weights according to Kahneman & Tversky (1992)
linear_in_log_odds_prob_weight <- ProbWeight(fun="linear_in_log_odds", 
                                             par=c(alpha=0.61, beta=0.724))

PTU_epsilon <- comparePT(choices_epsilon,
                 prob_weight_for_positive_outcomes=linear_in_log_odds_prob_weight,
                 prob_weight_for_negative_outcomes=linear_in_log_odds_prob_weight,
                 utility=tk_1992_utility, digits=4)
# For bug = $150
#   cid gid    ev    pt    ce    rp
# 1   1   1   559 219.4 457.6 101.4
# 2   1   2 636.5   233 489.9 146.6

# For bug = $500
#   cid gid   ev   pt   ce    rp
# 1   1   1 3730 1311 3490 240.3
# 2   1   2 3055 1006 2584 471.4
#

PTU_tau <- comparePT(choices_tau,
                 prob_weight_for_positive_outcomes=linear_in_log_odds_prob_weight,
                 prob_weight_for_negative_outcomes=linear_in_log_odds_prob_weight,
                 utility=tk_1992_utility, digits=4)
# For bug = $150
#   cid gid   ev    pt    ce    rp
# 1   1   1 1066   415 944.1 121.4
# 2   1   2 33.5 20.73 31.35 2.153

################################################################################
# Write our the compiler settings for reproducibility
writeLines(readLines(file.path(Sys.getenv("HOME"), ".R/Makevars")))
# CXXFLAGS=-O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function -Wno-macro-redefined