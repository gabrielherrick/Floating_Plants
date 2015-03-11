# Simulated Floating Plant Data
# Generate simulated data on mass, %cover, nplants, nutrient treatment. 
library(ggplot2)
library(dplyr)
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

#put deterministic projections in a data frame
det.df <- as.data.frame(cbind(week, rate.tmnts, pop.det))
det.df <- tbl_df(det.df)
det.df

# Make the stochastic part of the model
make.stochastic <- function(reps) {
  stoch.sim <- c(init.pops, rnorm(l.sim-3, pop.det[4:l.sim], 5))
}

# Apply the stochastic wrapper to the deterministic results
stoch.reps <- as.vector(replicate( 4, make.stochastic(4), simplify=T))

# I think for clarity, it might be nice to bind this together in a dataframe
# this is what I did after making the plot. 

# Make a dataframe to share
week.col <- rep(week, 4)
treat.col <- rep(groups, 4)
treat.col <- as.factor(rep(1:3, 24))
pop.col <- stoch.reps
pop.data <- as.data.frame(cbind(week.col, treat.col, pop.col))
pop.data <- tbl_df(pop.data)
pop.data <- rename(pop.data, week = week.col, nutrients = treat.col, 
                   population = pop.col)

pop.data


#Use this line to save output
#write.csv(pop.data, "simulated_data.csv")



# Using base plotting
plot(rep(week, 4), stoch.reps, ylab="Population Size", xlab = "Weeks")
lines(week, 10*rate.tmnts[1]^week, lty = 1, lwd = 2.5)
lines(week, 10*rate.tmnts[2]^week, lty = 2, lwd = 2.5)
lines(week, 10*rate.tmnts[3]^week, lty = 3, lwd = 2.5)

legend(0, 300, # places a legend at the appropriate place 
       c("r = 1.2","r = 1.8", "r = 2.0"), # puts text in the legend 
       lty=c(1, 2, 3), # gives the legend appropriate symbols (lines)
       lwd=c(2.5, 2.5, 2.5 )) # gives the legend lines the correct width

# Using qplot
qplot(week, population, data = pop.data) # very pretty, but can't add lines

# Make data for lines to be added. 
det.df
line1.2 <- det.df %>%
            filter(rate.tmnts == 1.2) %>%
            rename(line1.2 = pop.det)
line1.8 <- det.df %>%
            filter(rate.tmnts == 1.8) %>%
            select(line1.8=pop.det)          
line2.0 <- det.df %>%
            filter(rate.tmnts == 2.0) %>%
            select(line2.0=pop.det)
lines.df <- as.data.frame(cbind(line1.2, line1.8, line2.0))
lines.df <- tbl_df(lines.df)
lines.df

gplot1 <- ggplot() +
  geom_point(data = pop.data, aes(x=week, y=population))
gplot2 <- gplot1 +
  geom_line(data = lines.df, aes(x=week, y=line1.2, color= "line1.2")) +
  geom_line(data = lines.df, aes(x=week, y=line1.8, color = "line1.8")) +
  geom_line(data = lines.df, aes(x=week, y=line2.0, color = "line2.0")) 
gplot2

gplot3 <- gplot2 +
  scale_color_manual("Growth rate\n",labels = c("r=1.2", "r=1.8", "r=2.0"), 
                     values = c("blue", "green", "red")) +
  theme(text = element_text(size=20)) +
  ylab("population size") 
gplot3

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




