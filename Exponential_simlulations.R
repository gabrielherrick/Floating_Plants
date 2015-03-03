# Simulated Floating Plant Data
# Generate simulated data on mass, %cover, nplants, nutrient treatment. 

# Generate a sequence of dates
Dates <- seq.Date(as.Date("2015-01-13"), as.Date("2015-02-24"), by="week") 

# simulate everything as exponentially increasing population where
# population.time.t = initial.population + initial.popluation*rate^time
set.seed(147)

# Growth rates for three treatments
rate.tmnts <- c( 1.2, 1.8, 2)

# Time: 6 sampling times (0:5) each repeated 12 times for all reps and tmnts, 
week <- rep(0:5, each = 3)
num.weeks <- 6

# Group factor
groups <- rep(1:3, num.weeks) 

# Initial population sizes
init.pops <- rep(10, 3)

# Make the deterministic part of the model

pop.det<- init.pops * rate.tmnts[groups] ^ week
pop.det
plot(week, pop.det)
l.sim <- length(pop.det)

# Make the stochastic part of the model
make.stochastic <- function(reps) {
  stoch.sim <- c(init.pops, rnorm(l.sim-3, pop.det[4:l.sim], 5))
}

# Apply the stochastic wrapper to the deterministic results
stoch.reps <- as.vector(replicate( 4, make.stochastic(4), simplify=T))
plot(rep(week, 4), stoch.reps, ylab="Population Size", xlab = "Weeks")
lines(week, 10*rate.tmnts[1]^week, lty = 1, lwd = 2.5)
lines(week, 10*rate.tmnts[2]^week, lty = 2, lwd = 2.5)
lines(week, 10*rate.tmnts[3]^week, lty = 3, lwd = 2.5)

legend(0, 300, # places a legend at the appropriate place 
       c("r = 1.2","r = 1.8", "r = 2.0"), # puts text in the legend 
       
       lty=c(1, 2, 3), # gives the legend appropriate symbols (lines)
       
       lwd=c(2.5, 2.5, 2.5 )) # gives the legend lines the correct width


# Output a dataframe to share
week.col <- rep(week, 4)
treat.col <- rep(groups, 4)
treat.col <- rep(1:3, 24)
pop.col <- stoch.reps
pop.data <- as.data.frame(cbind(week.col, treat.col, pop.col))
write.csv(pop.data, "simulated_data.csv")




