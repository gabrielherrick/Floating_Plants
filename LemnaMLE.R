# MLE Lemna
library(dplyr)
setwd("~/Documents/Floating_Plants")
# Enter in the data
full.data <- read.csv( "Full_lemna_data.csv", header = T)
lemna <- tbl_df(full.data)
#
# Select chosen data
# Use dplyr tool "select" and rename columns in one go. 
lemna <- select(lemna, week = Week, treatment = Treatment, pop.size = Pop.size)
#
lemna.control <- filter(lemna, treatment == "C")
population <- lemna.control$pop.size
time <- lemna.control$week




##############################################
# 1.0 # CONTROL
##############################################

#################
# 1.1.0 # Exponential MLE
#################
# 1.1.1 # Make the likelihood function
LL.ec <- function(rate, mu, sigma) {
  # http://www.r-bloggers.com/fitting-a-model-by-maximum-likelihood/
  # Find residuals
  # for us, y = population, x = time. Parameters beta1 and beta0 are for a 
  # linear model where if y=mx+b, m = beta1 and b = beta0. These are the params
  # to be optimized, along with the parameters of the normal distribution in the 
  # next line. 
  # R = y - x * beta1 - beta0
  R <- population - (10*exp(rate*time))
  # 
  # Calculate the likelihood for the residuals (with mu and sigma as parameters)
  #
  R = suppressWarnings(dnorm(R, mu, sigma, log=TRUE))
  #
  # Sum the log likelihoods for all of the data points
  #
  -sum(R)
}
# 1.1.2 # Fit the paramters using MLE
library(bbmle)
fit.ec <- mle2(LL.ec, start = list(rate = .1, mu = 0, sigma = 20))
# See the results
summary(fit.ec)
# assign names to results
params.ec <- coef(fit.ec)
time.x <- 0:5
rate.fit.ec <- params.ec[1]
K.fit.ec <- params.ec[2]
#
# 1.1.3 # Make a function to plot MLE parameters in exponential model
exp.ec <- function(x) {10*exp(rate.fit.ec*x)}
#
# Plot it up
#
library(ggplot2)
base.plot <- ggplot(lemna.control, aes(x=time, y=population))+
  geom_point(size=4.5)
ec.plot <- base.plot +
  theme(axis.title.x = element_text( size=20))+
  theme(axis.title.y = element_text(size=20))+
  theme(axis.text.x  = element_text(size=16))+
  theme(axis.text.y  = element_text(size=16))+
  stat_function(fun=exp.ec, color = "#E69F00", size = 1.5)+
  #geom_line(aes(x=time.x, y= y.ec, color = "red"))+
  annotate("text", x = 2.5, y = 8, label = "Exponential; r = 0.14", 
           color = "#E69F00", size=8)
  #theme(legend.position="none")



#
#######################
# 1.2.0 # MLE Logistic 
#######################
#
# 1.2.1 # Likelihood function
LL.log <- function(rate, K, mu, sigma) {
  # http://www.r-bloggers.com/fitting-a-model-by-maximum-likelihood/
  # Use logistic formula from Case 2000
  logistic <- K/(1+((K-n.zero)/n.zero)*exp(-rate*time))
  ###
  # the numerator/denominator method also works #
  # num <- 10*exp(rate*time)                    #
  # denom <- 1+(exp(rate*time)-1)*(10/K)        #
  ### 
  #   Find residuals        
  R <- population - logistic
  # Calculate the likelihood for the residuals (with mu and sigma as parameters)
  #
  R <- suppressWarnings(dnorm(R, mu, sigma, log=TRUE))
  #
  # Sum the log likelihoods for all of the data points
  #
  -sum(R)
}

# 1.2.2 # Fit the paramters using MLE
fit.lc <- mle2(LL.lc, start = list(rate = 1, K = 22, mu = 0, sigma = 5))
summary(fit.lc)
params.lc <- coef(fit.lc)
time.x <- 0:5
rate.fit.lc <- params.lc[1]
K.fit.lc <- params.lc[2]
#lc.y <- (K.fit.lc*10*exp(rate.fit.lc*time.x))/
 # (K.fit.lc+10*(exp(rate.fit.lc*time.x)-1))
#

# Make a function to plot MLE parameters in logistic model
log.ec <- function(x) {
  num <- 10*exp(rate.fit.lc*x)
  denom <- 1+(exp(rate.fit.lc*x)-1)*(10/K.fit.lc) 
  num/denom
}

#
# Add logisitic model to control plot
#
lc.plot <- ec.plot +
  stat_function(fun=log.ec, color = "#56B4E9", size = 1.5)+
   annotate( "text", x=2.5, y=32, label= "Logistic; r= 1.14, K = 24", 
            color="#56B4E9", size=8)
lc.plot
#

# Do AIC
AICctab(fit.lc, fit.ec, nobs = 24, weights = TRUE)

##################################################
# LOW NUTRIENTS
##################################################
#
lemna.low <- filter(lemna, treatment == "L")
population <- lemna.low$pop.size
time <- lemna.low$week
#
#################
# Exponential MLE LOW
#################
# Make the likelihood function
LL.el <- function(rate, mu, sigma) {
  # http://www.r-bloggers.com/fitting-a-model-by-maximum-likelihood/
  # rate is the param to be optimized
  # Find residuals
  # 
  R <- population - (10*exp(rate*time))
  # 
  # Calculate the likelihood for the residuals (with mu and sigma as parameters)
  #
  R = suppressWarnings(dnorm(R, mu, sigma, log=TRUE))
  #
  # Sum the log likelihoods for all of the data points
  #
  -sum(R)
}
# Fit the paramters using MLE
exponential <- mle2(LL.el, start = list(rate = .1, mu = 0, sigma = 20))
# See the results
summary(exponential)
# assign names to results
params.el <- coef(exponential)
time.x <- 0:5
rate.fit.el <- params.el[1]
K.fit.el <- params.el[2]
#
# Make a function to plot MLE parameters in exponential model
exp.el <- function(x) {10*exp(rate.fit.el*x)}
#
# Plot it up
#
base.plot <- ggplot(lemna.low, aes(x=time, y=population))+
  geom_point(size=4.5)
el.plot <- base.plot +
  theme(axis.title.x = element_text( size=20))+
  theme(axis.title.y = element_text(size=20))+
  theme(axis.text.x  = element_text(size=16))+
  theme(axis.text.y  = element_text(size=16))+
  stat_function(fun=exp.el, color = "#E69F00", size = 1.5)+
  #geom_line(aes(x=time.x, y= y.ec, color = "red"))+
  annotate("text", x = 2.5, y = 8, label = "Exponential; r = 0.58", 
           color = "#E69F00", size=8)
#theme(legend.position="none")



#
#######################
# MLE Logistic LOW
#######################
#

#
LL.lc <- function(rate, K, mu, sigma) {
  # http://www.r-bloggers.com/fitting-a-model-by-maximum-likelihood/
  # Find residuals
  # for us, y = population, x = time. Parameters beta1 and beta0 are for a 
  # linear model where if y=mx+b, m = beta1 and b = beta0. These are the params
  # to be optimized, along with the parameters of the normal distribution in the 
  # next line. 
  # R = y - x * beta1 - beta0
  num <- 10*exp(rate*time)
  denom <- 1+(exp(rate*time)-1)*(10/K)
  #           
  R <- population - (num/denom)
  
  #R <- population - (K*10*exp(rate*time))/(K+10*(exp(rate*time)-1))
  # 
  # Calculate the likelihood for the residuals (with mu and sigma as parameters)
  #
  R <- suppressWarnings(dnorm(R, mu, sigma, log=TRUE))
  #
  # Sum the log likelihoods for all of the data points
  #
  -sum(R)
}
#

#
logistic <- mle2(LL.lc, start = list(rate = 1, K = 300, mu = 0, sigma = 20))
summary(logistic)
params.low <- coef(logistic)
time.x <- 0:5
rate.fit.low <- params.low[1]
K.fit.low <- params.low[2]
#
# Make a function to plot MLE parameters in logistic model
log.low <- function(x) {
  num <- 10*exp(rate.fit.low*x)
  denom <- 1+(exp(rate.fit.low*x)-1)*(10/K.fit.low) 
  num/denom
}

#
# Add logisitic model to control plot
#
low.plot <- el.plot +
  stat_function(fun=log.low, color = "#56B4E9", size = 1.5)+
  annotate( "text", x=2.5, y=155, label= "Logistic; r= 0.77, K = 287", 
            color="#56B4E9", size=8)
low.plot
#

# Do AIC
AICctab(fit.low, fit.el, nobs = 24, weights = TRUE)



##################################################
# HIGH NUTRIENTS
##################################################
#
lemna.high <- filter(lemna, treatment == "H")
population <- lemna.high$pop.size
time <- lemna.high$week
#
#################
# Exponential MLE
#################
# Make the likelihood function
LL.exp.high <- function(rate, mu, sigma) {
  # http://www.r-bloggers.com/fitting-a-model-by-maximum-likelihood/
  # rate is the param to be optimized
  # Find residuals
  # 
  R <- population - (10*exp(rate*time))
  # 
  # Calculate the likelihood for the residuals (with mu and sigma as parameters)
  #
  R = suppressWarnings(dnorm(R, mu, sigma, log=TRUE))
  #
  # Sum the log likelihoods for all of the data points
  #
  -sum(R)
}
# Fit the paramters using MLE
exponential <- mle2(LL.exp.high, start = list(rate = .7, mu = 29, sigma = 42))
# See the results
summary(exponential)
# assign names to results
params.exp.high <- coef(exponential)
time.x <- 0:5
rate.exp.high <- params.exp.high[1]
K.exp.high <- params.exp.high[2]
#
# Make a function to plot MLE parameters in exponential model
exp.high <- function(x) {10*exp(rate.exp.high*x)}
#
r.exp <- .7
# an experimental function to set start values
exp.exp <- function(x) {10*exp(r.exp*x)}
#
# Plot it up
#
base.plot <- ggplot(lemna.high, aes(x=time, y=population))+
  geom_point(size=4.5)
high.plot1 <- base.plot +
  theme(axis.title.x = element_text( size=20))+
  theme(axis.title.y = element_text(size=20))+
  theme(axis.text.x  = element_text(size=16))+
  theme(axis.text.y  = element_text(size=16))+
  stat_function(fun=exp.high, color = "#E69F00", size = 1.5)+
  annotate("text", x = 2.5, y = 8, label = "Exponential; r = 0.66", 
           color = "#E69F00", size=8)
high.plot1  
#theme(legend.position="none")



#
#######################
# MLE Logistic 
#######################
#

#
LL.log.high <- function(rate, K, mu, sigma) {
  # http://www.r-bloggers.com/fitting-a-model-by-maximum-likelihood/
  # Find residuals
  # for us, y = population, x = time. Parameters beta1 and beta0 are for a 
  # linear model where if y=mx+b, m = beta1 and b = beta0. These are the params
  # to be optimized, along with the parameters of the normal distribution in the 
  # next line. 
  # R = y - x * beta1 - beta0
  num <- 10*exp(rate*time)
  denom <- 1+(exp(rate*time)-1)*(10/K)
  #           
  R <- population - (num/denom)
  
  #R <- population - (K*10*exp(rate*time))/(K+10*(exp(rate*time)-1))
  # 
  # Calculate the likelihood for the residuals (with mu and sigma as parameters)
  #
  R <- suppressWarnings(dnorm(R, mu, sigma, log=TRUE))
  #
  # Sum the log likelihoods for all of the data points
  #
  -sum(R)
}
#

#
logistic <- mle2(LL.log.high, start = list(rate = 1.1, K = 320, mu = 0, sigma = 23))
summary(logistic)
params.high <- coef(logistic)
time.x <- 0:5
rate.fit.high <- params.high[1]
K.fit.high <- params.high[2]
#
# Make a function to plot MLE parameters in logistic model
log.high <- function(x) {
  num <- 10*exp(rate.fit.high*x)
  denom <- 1+(exp(rate.fit.high*x)-1)*(10/K.fit.high) 
  num/denom
}

# To experiment with plotting values to feed to MLE start
rate.log <- 1.1
K.log <- 320

log.log <- function(x){
  num <- 10*exp(rate.log*x)
  denom <- 1+(exp(rate.log*x)-1)*(10/K.log) 
  num/denom 
}
# end experiment. 

#
# Add logisitic model to control plot
#

high.plot2 <- high.plot1 +
  stat_function(fun=log.high, color = "#56B4E9", size = 1.5)+
  annotate( "text", x=1.5, y=260, label= "Logistic; r= 1.1, K = 321", 
            color="#56B4E9", size=8)

png("LemnaHighFit.png")
high.plot2
dev.off()

#

# Do AIC
AICctab(logistic, exponential, nobs = 24, weights = TRUE)




















