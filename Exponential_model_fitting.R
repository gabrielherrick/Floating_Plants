# Exponential_model_fitting.R
# I want to fit parameters to an exponential model (in particular the growth
# rate r). To start with, I will fit each treatment one at a time. I'm not sure
# if there might be a way to automate fit to each treatment using a grouping
# variable (there probably is), but it seems more straightforward to brute 
# force things. "Premature optimization is the root of all evil" -Donald Knuth


# Enter in the data
full.data <- read.csv ( "Full_lemna_data.csv", header = T)
# Select chosen data
select.data <- as.data.frame( cbind( full.data$Week, full.data$Treatment, 
                                     full.data$Pop.size))
names(select.data) <- c("week", "treatment", "pop.size")
# Extract values of population size that correspond to low nutrient treatment
low.data <- select.data[select.data$treatment == 3, ]
# Time will be our x-axis independent variable
time <- low.data$week
# Population size will be our y-axis dependent variable
size <- low.data$pop.size

# Begin modeling size as a function of time

# Let's take a look at the data
plot(time, size)


# Start by seeing if this is linear

linear.fit <- lm(size ~ time)
summary(linear.fit) 
# A good explanation for the output of this can be found
# here: http://blog.yhathq.com/posts/r-lm-summary.html

# Plot size data with linear prediction
plot(time, size)
abline(linear.fit)
# You can see here that something is going on where the residuals aren't evenly
# dispersed on either side of the best fit line. 

# Explore residuals: The residuals are the difference between the actual values 
# of the variable you're predicting and predicted values from your 
# regression--y - ŷ

size.res = resid(linear.fit)
plot( time, size.res)
abline(0,0)

orig.par <- par()

par(mar = c(5, 5, 2, 2))
par(mfrow=c(2,2)) # Sets figure creation to make four plots in one page
plot(linear.fit) # 

par(orig.par) # Returns figure creation to normal





