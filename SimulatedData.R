# Simulated data
# Simulated Floating Plant Data
# Generate simulated data on mass, %cover, nplants, nutrient treatment. 
library(ggplot2)
library(dplyr)

##############
# Exponential
##############
# We want to make a simulated data set with time and population size

time <- 0:5
n.zero <- 10
rate <- 0.7
pop.size <- n.zero*exp(rate*time)
pop.size

df <- data.frame(time, pop.size)
theme_set(theme_bw(base_size=20))
plot1 <- ggplot(data=df, aes(x=time, y=pop.size))+
  geom_point(shape=1, size=3)
plot1

# Make a function that adds stochasticity
make.stochastic <- function(reps) {
  # makes population sizes stochastic
  # Vars
  # reps = number of stochastic reps to make
  stoch.sim <- c(n.zero, rnorm(n=length(time)-1, mean=pop.size, sd=20))
}

# Apply the stochastic wrapper to the deterministic results
stoch.reps <- as.vector(replicate(4, make.stochastic(4), simplify=T))
week.col <- rep(time, 4)
df2 <- data.frame(week.col, stoch.reps)

plot2 <- ggplot(data=df2, aes(x=week.col, y=stoch.reps))+
  geom_point(shape=1, size=3)+
  xlab("Week")+
  ylab("Population size")
  theme(axis.title.x=element_text)
plot2

png("SimExp_nolines.png")
# or tiff("plot.tiff")
print(plot2)
dev.off()

# Do MLE
population <- stoch.reps # just renaming to fit into copied function
time <- week.col # renaming
#################
# Exponential MLE
#################
# Make the likelihood function
LL.exp <- function(rate, mu, sigma) {
  # http://www.r-bloggers.com/fitting-a-model-by-maximum-likelihood/
  # Find residuals: subtract stochastic simulation from exponential model
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
library(bbmle)
fit.exp <- mle2(LL.exp, start = list(rate = .6, mu = 0, sigma = 20))
# See the results
summary(fit.exp)
# assign names to results
params.exp <- coef(fit.exp)
time.x <- 0:5
rate.fit.exp <- params.exp[1]
K.fit.exp <- params.exp[2]
#
# Make a function to plot MLE parameters in exponential model
exp.fun <- function(x) {10*exp(rate.fit.exp*x)}
#
# Plot it up
plot3 <- plot2 +
  stat_function(fun=exp.fun, color = "#E69F00", size = 1.5)+
  #geom_line(aes(x=time.x, y= y.ec, color = "red"))+
  annotate("text", x = 2.5, y = 125, label = "Exponential; r = 0.54", 
           color = "#E69F00", size=8)
plot3

png("SimExp_withlines.png")
# or tiff("plot.tiff")
print(plot3)
dev.off()

############
# Logistic #
############

# Simulate logistic population growth
time <- 0:5
n.zero <- 10
rate <- 0.7
K <- 200
log.pop <- K/(1+((K-n.zero)/n.zero)*exp(-rate*time))
log.pop
qplot(x=time, y=log.pop)

# Add stochastic wrapper
# Picked this up from Bolker Ecological Models
# Make a function that adds stochasticity to logistic pop sizes
make.stochastic <- function(reps) {
  # makes population sizes stochastic
  # Vars
  # reps = number of stochastic reps to make
  stoch.sim <- c(n.zero, rnorm(n=length(time)-1, mean=log.pop, sd=log.pop/10))
}

# Apply the stochastic wrapper to the deterministic results
log.stoch <- as.vector(replicate(4, make.stochastic(4), simplify=T))
week.col <- rep(time, 4)
df.log <- data.frame(week.col, log.stoch)

plot.log <- ggplot(data=df.log, aes(x=week.col, y=log.stoch))+
  geom_point(shape=1, size=3)+
  xlab("Week")+
  ylab("Population size")
plot.log

png("SimLog_nolines.png")
# or tiff("plot.tiff")
print(plot.log)
dev.off()

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

#
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

