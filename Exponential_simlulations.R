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
#plot(week, pop.det)
l.sim <- length(pop.det)

#put deterministic projections in a data frame
det.df <- as.data.frame(cbind(week, rate.tmnts, pop.det))
det.df <- tbl_df(det.df)
det.df

# Make the stochastic part of the model
# don't make starting population sizes stochastic, so  c() init.pops to
# pop.det minus first three values
make.stochastic <- function(reps) {
  stoch.sim <- c(init.pops, rnorm(n=l.sim-3, mean=pop.det[4:l.sim], sd=20))
}

# Apply the stochastic wrapper to the deterministic results
stoch.reps <- as.vector(replicate( 4, make.stochastic(4), simplify=T))

# I think for clarity, it might be nice to bind this together in a dataframe
# this is what I did after making the plot. 

# Make a dataframe
week.col <- rep(week, 4)
treat.col <- rep(groups, 4)
treat.col <- factor(rep(1:3, 24))
pop.col <- stoch.reps
pop.data <- as.data.frame(cbind(week.col, treat.col, pop.col))
pop.data <- tbl_df(pop.data)
pop.data <- rename(pop.data, week = week.col, nutrients = treat.col, 
                   population = pop.col)

#Use this line to save output
#write.csv(pop.data, "simulated_data.csv")

gplot1 <- ggplot() +
  geom_point(data = pop.data, aes(x=week, y=population, 
                                  shape=as.factor(nutrients)))
gplot1

gplot1.5 <- gplot1+
  geom_line(data=det.df, aes(x=week, y=pop.det, group=rate.tmnts))
gplot1.5 # this throws in straight little line segments. Maybe I should use MLE
# to find exponential model parameters. 
gplot2 <- gplot1 +
  geom_line(data = lines.df, aes(x=week, y=line1.2, 
                                 linetype= "line1.2")) +
  geom_line(data = lines.df, aes(x=week, y=line1.8,  
                                 linetype="line1.8")) +
  geom_line(data = lines.df, aes(x=week, y=line2.0,  
                                 linetype="line2.0")) 
gplot2

gplot3 <- gplot2 +
  #scale_color_manual("Growth rate\n",labels = c("r=1.2", "r=1.8", "r=2.0"), 
                     #values = c("blue", "green", "red")) +
  theme(text = element_text(size=20)) +
  ylab("population size") +
  theme_bw(base_size=20)+
  theme(legend.position=c(.25, .50))+
  scale_fill_discrete(name="Experimental\nCondition")
gplot3

# Plot of simulated exponential data
png("SimulatedExponential.png")
# or tiff("plot.tiff")
print(gplot1)
dev.off()

# Plot of simulated exponential with model fits
png("SimulatedExponentialLines.png")
# or tiff("plot.tiff")
print(gplot3)
dev.off()





# Output a dataframe to share
week.col <- rep(week, 4)
treat.col <- rep(groups, 4)
treat.col <- rep(1:3, 24)
pop.col <- stoch.reps
pop.data <- as.data.frame(cbind(week.col, treat.col, pop.col))
write.csv(pop.data, "simulated_data.csv")




