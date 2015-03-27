# RealPlantMLE
# This script does the following: 
# 1.0 loads data from csv, 
# 2.0 fits parameters for exponential growth using  MLE 
# 3.0 fits parameters for logistic growth using MLE
# 4.0 plots all data and models
# To do: wrap in function. then can write script which reads and selects data
# function will take whatever selected data in the form of the population 
# and time... not sure if that will work given need to provide good start values
# for MLE. maybe write separate scripts for each nutrient level and species

# Lemna Control, Lemna Low, Lemna High, ...
# I think I'd rather have 9 scripts that were under 150 lines each than a single
# script over 1000 lines. Those big scripts get hard to deal with. 

# 0.0 # Do preliminaries
library(ggplot2)
library(dplyr)
library(bbmle)
setwd("~/Documents/Floating_Plants")

# 1.0 # Load data
# 1.1 # Get data
full.data <- read.csv( "Full_lemna_data.csv", header = T)
lemna <- tbl_df(full.data)

# 1.2 # Select chosen data
# Use dplyr tool "select" and rename columns in one go. 
lemna <- select(lemna, time  = Week, treatment = Treatment, population = Pop.size)
lemna.control <- filter(lemna, treatment == "C")
population <- lemna.control$population
time <- lemna.control$time 

# 1.3.0 # Plot raw data
plot.pop <- ggplot(data=lemna.control, aes(x=time , y=population))+
  geom_point(shape=1, size=3)+
  xlab("Week")+
  ylab("Lemna population size")
plot.pop

# 1.3.1 # Save the plot to file
png("Lemna_Control_nolines.png")
# or tiff("plot.tiff")
print(plot.pop)
dev.off()

# 2.0 # Exponential model
# 2.1 # Make the likelihood function
LL.exp <- function(rate, mu, sigma) {
  # http://www.r-bloggers.com/fitting-a-model-by-maximum-likelihood/
  # Find residuals: subtract stochastic simulation from exponential model
  R <- population - (10*exp(rate*time))
  # Calculate the likelihood for the residuals (with mu and sigma as parameters)
  R = suppressWarnings(dnorm(R, mu, sigma, log=TRUE))
  # Sum the log likelihoods for all of the data points
  -sum(R)
}
# 2.2 # Fit the paramters using MLE
fit.exp <- mle2(LL.exp, start = list(rate = .1, mu = 0, sigma = 20))
# See the results
summary(fit.exp)

# assign names to results
params.exp <- coef(fit.exp)
time.x <- 0:5
rate.fit.exp <- params.exp[1]
K.fit.exp <- params.exp[2]

# 2.3 # Make a function to plot MLE parameters in exponential model
exp.fun <- function(x) {10*exp(rate.fit.exp*x)}
#
# 2.4 # Plot it up
plot.exp.line <- plot.pop +
  stat_function(fun=exp.fun, color = "#E69F00", size = 1.5)+
  #geom_line(aes(x=time.x, y= y.ec, color = "red"))+
  annotate("text", x = 1.5, y = 30, label = "Exponential; r = 0.14", 
           color = "#E69F00", size=8)
plot.exp.line

png("Lemna_Control_ExpLine.png")
# or tiff("plot.tiff")
print(plot.exp.line)
dev.off()

# 3.0 # logistic model
n.zero <- 10
# 3.1 # Likelihood function
LL.log <- function(rate, K, mu, sigma) {
  # http://www.r-bloggers.com/fitting-a-model-by-maximum-likelihood/
  # Use logistic formula from Case 2000
  logistic <- K/(1+((K-n.zero)/n.zero)*exp(-rate*time))
  ###
  #   Find residuals        
  R <- population - logistic
  # Calculate the likelihood for the residuals (with mu and sigma as parameters)
  R <- suppressWarnings(dnorm(R, mu, sigma, log=TRUE))
  # Sum the log likelihoods for all of the data points
  -sum(R)
}
#

# 3.2 # MLE
fit.log <- mle2(LL.log, start = list(rate = 1, K = 24, mu = 0, sigma = 20))
# see results
summary(fit.log)
# Assign names to results
params.log <- coef(fit.log)
time.x <- 0:5
rate.fit.log <- params.log[1]
K.fit.log <- params.log[2]

# 3.3 # Make a function to plot MLE parameters in logistic model
log.fun <- function(time) {
  logistic <- K.fit.log/(1+((K.fit.log-n.zero)/n.zero)*exp(-rate.fit.log*time))
}

# 3.4 # Plot it up

plot.log.line <- plot.pop +
  stat_function(fun=log.fun, color = "#56B4E9", size = 1.5)+
  annotate( "text", x=1.5, y=32, label= "Logistic; r= 1.14, K = 24", 
            color="#56B4E9", size=8)
plot.log.line

png("Lemna_Control_LogLine.png")
# or tiff("plot.tiff")
print(plot.log.line)
dev.off()

#

# Do AIC
AICctab(fit.log, fit.exp, nobs = 24, weights = TRUE)








