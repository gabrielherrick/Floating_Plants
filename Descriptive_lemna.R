# Descriptive stats on Lemna

# Enter in the data
full.data <- read.csv ( "Full_lemna_data.csv", header = T)
# Select chosen data
select.data <- as.data.frame( cbind( full.data$Week, full.data$Treatment, 
                                     full.data$Pop.size))
names(select.data) <- c("week", "treatment", "pop.size")
# one peculiarity here is that R decided to turn "treatment" into a factor
# variable (which it is), but then it assigned the numbers 1,2,3 to C, H, L 
# in that order, which is alphabetical. so Control = 1, Low = 3, High = 2. 
# get the mean number of plants at the end of the experiment for each treatment.

mean.table <- with(select.data, (tapply(pop.size, list("nutrient"=treatment, 
                                                     "week"=week), mean)))
mean.table
# This table shows us means for each treatment in rows across each week
# in columns. 

# View the ranges of the data 
range.table <- with(select.data, (tapply(pop.size, list("nutrient"=treatment, 
                                                        "week"=week), range)))
range.pretty <- as.data.frame(range.table)
names(range.pretty) <- c("week 0", "week 1", "week 2", "week 3", "week 4", 
                         "week 5")
row.names(range.pretty) <- c( "Control", "High", "Low")
range.pretty

# The five-number summary
summary.table <- with(select.data, (tapply(pop.size, list("nutrient"=treatment, 
                                                        "week"=week), summary)))
summary.pretty <- as.data.frame(summary.table)
names(summary.pretty) <- c("week 0", "week 1", "week 2", "week 3", "week 4", 
                         "week 5")
row.names(summary.pretty) <- c( "Control", "High", "Low")
# This is pretty ugly in R output
# Perhaps it would be better to just look at a little of the data at a time? 
# Here is the summary data for the last week
summary.pretty[, 6]


boxplot(pop.size ~ treatment*week, data=select.data)
# This is a lot to look at in one plot. Maybe it makes more sense to look
# at an individual week or two

# Week 2
week2.data <- select.data[select.data$week==2, ]
boxplot(pop.size ~ treatment*week, data=week2.data, 
        names = c("Tap water", "High", "Low"), ylab = "Number of plants", 
        xlab = "Nutrient Levels", 
        main = "Population sizes after 2 weeks of growth")



