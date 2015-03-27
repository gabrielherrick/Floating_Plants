#Logistic modeling Lemna

library(dplyr)
setwd("~/Documents/Floating_Plants")
# Enter in the data
full.data <- read.csv( "Full_lemna_data.csv", header = T)
lemna <- tbl_df(full.data)
lemna # should print nicely

# Select chosen data
# Use dplyr tool "select" and rename columns in one go. 
lemna <- select(lemna, week = Week, treatment = Treatment, pop.size = Pop.size)
lemna

# Select low treatment 
lemna.low <- filter(lemna, treatment == "L")
lemna.low
population <- lemna.low$pop.size
population
time <- lemna.low$week
time

# Model exponential growth
exponential.model <- nls(population ~ 10*rate^time, 
                         start = list(rate = 1.9))
summary(exponential.model)

#exponential continuous
exp.cont <- nls(population ~ 10*exp(rate*time),
                start = list(rate = 1.8))
summary(exp.cont)  

time.x <- seq(0,5,by=.1)
plot(y=population, x= time)
lines(x=time.x, y= predict(exp.cont, list(time=time.x)))

# logistic continuous
logisticModel <- nls(population~(K*10*exp(rate*time))/(K+10*(exp(rate*time)-1)), 
                     start=list(rate=.8, K=250))
summary(logisticModel)

plot(y=population, x = time)
lines(x=time.x, y= predict(logisticModel, list(time=time.x)))

# Repeat with high

# Select high treatment 
lemna.high <- filter(lemna, treatment == "H")
lemna.high
population <- lemna.high$pop.size
population
time <- lemna.high$week
time

#exponential high
exp.high <- nls(population ~ 10*exp(rate*time),
                start = list(rate = 1.8))
summary(exp.high)  

time.x <- seq(0,5,by=.1)
plot(y=population, x= time)
lines(x=time.x, y= predict(exp.high, list(time=time.x)))

# logistic high
# logistic continuous
logisticModel <- nls(population~(K*10*exp(rate*time))/(K+10*(exp(rate*time)-1)), 
                     start=list(rate=.8, K=250))
summary(logisticModel)

plot(y=population, x = time)
lines(x=time.x, y= predict(logisticModel, list(time=time.x)))

# Select control

lemna.control <- filter(lemna, treatment == "C")
population <- lemna.control$pop.size

time <- lemna.control$week
time

#exponential control
exp.control <- nls(population ~ 10*exp(rate*time),
                start = list(rate = 1.8))
summary(exp.control)  

time.x <- seq(0,5,by=.1)
plot(y=population, x= time)

lines(x=time.x, y= predict(exp.control, list(time=time.x)), lty=2)

# logistic control
logisticModel <- nls(population~(K*10*exp(rate*time))/(K+10*(exp(rate*time)-1)), 
                     start=list(rate=.8, K=50))
summary(logisticModel)

plot(y=population, x = time)
lines(x=time.x, y= predict(logisticModel, list(time=time.x)), col = "red")

# TODO(3-11-15): Above all needs to be cleaned up and put in to Rmd and 
# presentation
y <- population
x <- time


# MLE Logistic Control
LL <- function(rate, K, mu, sigma) {
  # http://www.r-bloggers.com/fitting-a-model-by-maximum-likelihood/
  # Find residuals
  # for us, y = population, x = time. Parameters beta1 and beta0 are for a 
  # linear model where if y=mx+b, m = beta1 and b = beta0. These are the params
  # to be optimized, along with the parameters of the normal distribution in the 
  # next line. 
  # R = y - x * beta1 - beta0
   R <- y - (K*10*exp(rate*time))/(K+10*(exp(rate*time)-1))
  # 
  # Calculate the likelihood for the residuals (with mu and sigma as parameters)
  #
  R = suppressWarnings(dnorm(R, mu, sigma))
  #
  # Sum the log likelihoods for all of the data points
  #
  -sum(log(R))
}
library(bbmle)

fit <- mle2(LL, start = list(rate = 1, K = 22, mu = 0, sigma = 5))
summary(fit)
params <- coef(fit)

rate.fit <- params[1]
K.fit <- params[2]
plot(y=population, x = time)
lines(x=time.x, 
      y= (K.fit*10*exp(rate.fit*time.x))/(K.fit+10*(exp(rate.fit*time.x)-1)), 
      col = "blue")



