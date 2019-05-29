# Copyright Richard Torkar
# Analysis of validation for the paper:
# Arguing Practical Significance in Software Engineering Using Bayesian Data 
# Analysis

# In this file we will do ordlogit analysis (i.e., analysis of ordered 
# categorical outcomes such as Likert scale data)

library(readxl)
library(rethinking)
library(tidyr)

d <- read_xlsx("data.xlsx")

# Categories we should compare (column name and number):

# B (5) vs. H (11), i.e., how confident are you from 1--5?
# 1 – Not at all confident
# 2 – slightly confident
# 3 – somewhat confident
# 4 – Fairly confident
# 5 – Completely confident

# E (8) vs. K (14), i.e., how confident are you from 1--5?
# 1 – Not at all confident
# 2 – slightly confident
# 3 – somewhat confident
# 4 – Fairly confident
# 5 – Completely confident
#
# We also have Column 3 which we can use as predictor (# years in the company)

# For each of the above Likert questions, we ask how certain they are from 1 to 
# 5.

# We'll make use of the columns "Years in company" and "B", "E", "H", and "K". 
# Let's rename them to something sane (at least for me :)

colnames(d)[colnames(d)=="Years in company"] <- "experience"

# Let's rename the "how certain are you of your answer" suffixing it with 
# _L for Likert.
# First 
colnames(d)[colnames(d)=="B"] <- "ET_TRAD_L"
colnames(d)[colnames(d)=="E"] <- "ME_TRAD_L"
colnames(d)[colnames(d)=="H"] <- "ET_BA_L"
colnames(d)[colnames(d)=="K"] <- "ME_BA_L"

# Let's drop the rest of the columns
d <- d[-c(1,2,4,6,7,9,10,12,13,15)]

# Center and standardize our experience pedictor (only for the sake of 
# sampling)
d$experience_s <- standardize(d$experience)

# How many NAs?
sum(is.na(d))
# [1] 5
# So, we have 5 NAs, which we'll need to handle in a principled way, i.e., 
# since they are in the outcome variables we can simply remove those rows
################################################################################
# First a model for how certain they are to introduce ET

# Make the data long format
data_long <- gather(d, treatment, measurement, ET_TRAD_L, ET_BA_L)

# We need to build models where we use ordlogit regression (ordered categorical
# outcomes)
my_data <- data.frame(
  experience_s = data_long$experience_s,
  # Set ET_TRAD == 1 else 2
  treatment = as.integer(ifelse( data_long$treatment=="ET_TRAD_L" , 0 , 1 )),
  measure = as.integer(data_long$measurement)
)

# remove three NAs in this case
my_data_ET <- my_data[complete.cases(my_data), ]

# Let's use ulam() instead of map2stan here since it has better support for 
# ordered categories
m_ET_L <- ulam(
  alist(
    measure ~ dordlogit(phi, cutpoints),
    phi <- b_TREAT * treatment + b_EXP * experience_s,
    b_TREAT ~ dnorm(0, 0.5),
    b_EXP ~ dnorm(0, 0.5),
    cutpoints ~ dnorm(0, 1.5)
  ), data = my_data_ET#, chains = 4, cores = 4, iter=1e4
)

plot(precis(m_ET_L, depth=2, prob=0.95))
# On 95% HPDI we see experience as significant
################################################################################
# Second, a model for how certain they are to introduce more experienced testers

# Make the data long format
data_long <- gather(d, treatment, measurement, ME_TRAD_L, ME_BA_L)

my_data <- data.frame(
  experience_s = data_long$experience_s,
  # Set ME_TRAD_L == 0 else 1
  treatment = as.integer(ifelse( data_long$treatment=="ME_TRAD_L" , 0 , 1 )),
  measure = as.integer(data_long$measurement)
)

# remove two NAs in this case
my_data_ME <- my_data[complete.cases(my_data), ]

# Let's use ulam() instead of map2stan here since it has better support for 
# ordered categories
m_ME_L <- ulam(
  alist(
    measure ~ dordlogit(phi, cutpoints),
    phi <- b_TREAT * treatment + b_EXP * experience_s,
    b_TREAT ~ dnorm(0, 0.5),
    b_EXP ~ dnorm(0, 0.5),
    cutpoints ~ dnorm(0, 1.5)
  ), data = my_data_ME#, chains = 4, cores = 4, iter=1e4
)

plot(precis(m_ME_L, depth=2, prob=0.95))
# Same trends but here experience isn't significant anymore
################################################################################
# Let's plot and start with m_ME_L
post <- as.data.frame(extract.samples(m_ME_L))

plot( 1 , 1 , axes=F, type="n", main="", 
      xlab="treatment", ylab="probability",
      xlim=c(0,1), ylim=c(0,1), xaxp=c(0,1,1), yaxp=c(0,1,2))
axis(1, at = 0:1, label = c("traditional","decision tree"), tick=F)
axis(2, at=c(0,0.5,1), label= c("0", ".5", "1.0"), tick=F, las=2)

treatment <- 0:1
for ( s in 1:50 ) {
  p <- post[s,]
  ak <- as.numeric(p[1:4])
  phi <- p$b_TREAT * treatment
  pk <- pordlogit(1:4, a=ak, phi=phi )
  
  for ( i in 1:4 )
    lines(treatment, pk[,i], col=col.alpha(rangi2, 0.1) )
  
  abline(h=0, lty=2, col=col.alpha(rangi2, 0.1))
  abline(h=1, lty=2, col=col.alpha(rangi2, 0.1))
}
# So we have a tendency of higher prob for higher Likert values when using BA, 
# and the certainty is more expressive for BA (right hand sight where we see 
# values being pressed together at the top.)

# Same for ET model
post <- as.data.frame(extract.samples(m_ET_L))

plot( 1 , 1 , axes=F, type="n" , main="", 
      xlab="treatment" , ylab="probability" ,
      xlim=c(0,1) , ylim=c(0,1) , xaxp=c(0,1,1) , yaxp=c(0,1,2) )
axis(1, at = 0:1, label = c("traditional","proposal"), tick=F)
axis(2, at=c(0,0.5,1), label= c("0", ".5", "1.0"), tick=F, las=2)

treatment <- 0:1
for ( s in 1:50 ) {
  p <- post[s,]
  ak <- as.numeric(p[1:4])
  phi <- p$b_TREAT * treatment
  pk <- pordlogit(1:4, a=ak, phi=phi)
  
  for ( i in 1:4 )
    lines(treatment, pk[,i], col=col.alpha(rangi2, 0.1) )
  
  abline(h=0, lty=2, col=col.alpha(rangi2, 0.1))
  abline(h=1, lty=2, col=col.alpha(rangi2, 0.1))
  arrows(x0=0.1, y0=0.05, y1=0.7, code=3)
  arrows(x0=0.9, y0=0.55, y1=1.0, code=1)
}
# Same as previous. We see something clearly different between TRAD and BA.
