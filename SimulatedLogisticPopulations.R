#SimulatedLogisticPopulations
library(ggplot2)
library(dplyr)
library(bbmle)
theme_set(theme_bw(base_size=20)) # I like this theme better than default
set.seed(147)
# 1.0 # Simulate Logistic growth in a population. 
      # Picked this up from Bolker Ecological Models
# 1.1 # Simulate logistic population growth
time <- 0:5
n.zero <- 10
rate <- 1.2
K <- 300
log.pop <- K/(1+((K-n.zero)/n.zero)*exp(-rate*time))

# 1.2 # Make stochastic wrapper
      # Make a function that adds stochasticity to logistic pop sizes
make.stochastic <- function(reps) {
      # makes population sizes stochastic
      # reps = number of stochastic reps to make
  stoch.sim <- c(n.zero, rnorm(n=length(time)-1, mean=log.pop, sd=log.pop/5))
}

# 1.3 # Apply the stochastic wrapper to the deterministic results
population <- as.vector(replicate(4, make.stochastic(4), simplify=T))
week.col <- rep(time, 4)
df.log <- data.frame(week.col, population)

# 1.4 # Plot these simulated data
plot.log <- ggplot(data=df.log, aes(x=week.col, y=population))+
  geom_point(shape=1, size=3)+
  xlab("Week")+
  ylab("Simulated population size")
plot.log

# 1.5 # Save the plot to file
png("SimLog_nolines.png")
# or tiff("plot.tiff")
print(plot.log)
dev.off()

# 2.0 # Add in a maximum likelihood estimate of the exponential model to this 
      # logistically generated data. 

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
fit.exp <- mle2(LL.exp, start = list(rate = .6, mu = 0, sigma = 20))
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
plot.exp.line <- plot.log +
  stat_function(fun=exp.fun, color = "#E69F00", size = 1.5)+
  #geom_line(aes(x=time.x, y= y.ec, color = "red"))+
  annotate("text", x = 1.5, y = 200, label = "Exponential; r = 0.64", 
           color = "#E69F00", size=8)
plot.exp.line

png("SimLog_ExpLine.png")
# or tiff("plot.tiff")
print(plot.exp.line)
dev.off()

# 3.0 # Maximum likelihood estimate of logistic model
      # already did all the simulation above, just need to add MLE

# 3.1 # Likelihood function
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
#

# 3.2 # MLE
fit.log <- mle2(LL.log, start = list(rate = .91, K = 330, mu = 0, sigma = 20))
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

plot.log.line <- plot.log +
  stat_function(fun=log.fun, color = "#56B4E9", size = 1.5)+
  annotate( "text", x=1.5, y=200, label= "Logistic; r= 0.90, K = 336", 
            color="#56B4E9", size=8)
plot.log.line

png("SimLog_LogLine.png")
# or tiff("plot.tiff")
print(plot.log.line)
dev.off()

#

# Do AIC
AICctab(fit.log, fit.exp, nobs = 24, weights = TRUE)

