# Descriptive stats on Lemna
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


# Use grouping functions to let R know that treatment and week are grouping variables
by.treatment <- group_by(lemna, treatment, week)
by.treatment
pop <- summarize(by.treatment, number = mean(pop.size))
pop # This table shows us means for each treatment arranged by week. 
pop.range <- summarize(by.treatment, min = min(pop.size), max = max(pop.size))
pop.range # This shows us the range



# The five-number summary

summary.table <- with(by.treatment, (tapply(pop.size, list("nutrient"=treatment, 
                                                          "week"=week), summary)))
summary.pretty <- as.data.frame(summary.table)

names(summary.pretty) <- c("week 0", "week 1", "week 2", "week 3", "week 4", 
                         "week 5")
row.names(summary.pretty) <- c( "Control", "High", "Low")
# Let's look at a little of the data, 
# Here is the summary data for the last week
summary.pretty[, 6]


boxplot(pop.size ~ treatment*week, data=lemna)
# This is a lot to look at in one plot. Maybe it makes more sense to look
# at an individual week or two

# Week 2
week2.data <- filter( lemna, week==2)
week2.data

plotfactor <- as.factor(rep(c(1, 2, 3), 4))
boxplot(pop.size ~ plotfactor, data=week2.data, 
        names = c("Tap water", "Low", "High"), ylab = "Number of plants", 
        xlab = "Nutrient Levels", 
        main = "Population sizes after 2 weeks of growth")


# Convert treatment to NNCmax value

NNCmax <- rep(c(0, 0.188, 1.00), length.out=length(lemna$week))
lemna = tbl_df(cbind(lemna, NNCmax))

# plot week 4 population sizes
lemna5 <- filter(lemna, week == 4)

library(ggplot2)

p <- ggplot(lemna5, aes(NNCmax, (pop.size)))
            
p + geom_point(size=4) +
  ylab("Population Size") +
  theme(axis.title.x = element_text(face="bold", size=20, vjust=0),
        axis.text.x  = element_text(size=16), 
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(face = "bold", size = 20))+
  stat_smooth(method= "lm")

# The shaded area from stat_smooth is the pointwise 95% confidence interval
# this shows that this relationship might not be very linear, a conclusion further
# supported by plotting regression diagnostics below. 

# Linear regression
fit <- lm(pop.size~NNCmax, data=lemna5)
fit
summary(fit)

# Regression diagnostic plots
orig.par <- par()
par(mar = c(5, 5, 2, 2))
par(mfrow=c(2,2)) # Sets figure creation to make four plots in one page
plot(fit) # 

par(orig.par) # Returns figure creation to normal

# Take home, relationship is not linear. So what is the relationship? Let's think
# about the biology: these are growing populations. Maybe what we ought to 
# concern ourselves with are exponential population growth rates.








