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

###############################################
# INSTANTANEOUS R
#
# weights are relative probability that each model is the right one. So here, 
# we can be around 93% confident that the logistic is a better model than the 
# exponential. Looking at the plot, I am even more confident that the logistic 
# the better model. 
#
# This is going to give us a new fitted logistic and exponential model for each 
# Treatment, but will I then be able to say how nutrient increase from 0.188 NCC
# to 1 NCC affects growth rate paramter r? What we really want is a map of 
# NNC on the x-axis and paramters r and K on the y-axis. 
#
# To get that, we have to have multiple replicates of this whole experiment,
# because you can't estimate parameters based on data with one replicate. We use
# our four meager replicates to get our one estimate r and K for each treatment. 
# r=ln(lambda) where N(t) = lambda*N(t-1)
#
shift <- function(data, n){
  # shift the values of a vector in a data set
  # data = the vector to be shifted
  # n = the number of rows to shift
  # step 1: add three NA values at front 
  # step 2: drop the last three elements
  c(rep(NA,n), head(data, length(data)-n))
  # 
}
prev.pop.size <- shift(lemna$pop.size, 3)
lemna <- tbl_df(cbind(lemna, prev.pop.size))
#
lemna <- mutate(lemna, r.inst = log(pop.size/prev.pop.size))
# Now I want to do a model of r as a function of treatment. # I need to add in
# NNC score for treatment as when I did linear regression of pop sizes. 

# Convert treatment to NNCmax value

NNCmax <- rep(c(0, 0.188, 1.00), length.out=length(lemna$week))
lemna = tbl_df(cbind(lemna, NNCmax))

# select first two weeks as best estmiators of exponential growth
week12 <- c(1,2)
week23 <- c(2,3)
lemna2 <- filter(lemna, week %in% week12)
ggplot(lemna2, aes(x=NNCmax, y= r.inst))+
  geom_point()+
  geom_smooth(method = lm) +
  annotate("text", x = .5, y = .95, label = "r  = 0.35*NNCmax + 0.48")

fit.lm.r <- lm(r.inst~NNCmax, data=lemna2)
summary(fit.lm.r)

lemna3 <- filter(lemna, week %in% week23)
ggplot(lemna3, aes(x=NNCmax, y= exp(r.inst)))+
  geom_point()+
  geom_smooth(method = lm) +
  annotate("text", x = .5, y = .95, label = "r  = 0.48*NNCmax + 0.51")

fit.lm.r <- lm(exp(r.inst)~NNCmax, data=lemna3)
summary(fit.lm.r)