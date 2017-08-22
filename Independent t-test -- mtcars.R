## ------------------------------------------------------------------------
#Display the first few rows of the data to understand its structure
head(mtcars)

## ------------------------------------------------------------------------
# The am variable is coded as 0/1. Give it meaningful labels
mtcars$am = factor(mtcars$am, levels = c(0,1), labels = c("Automatic", "Manual"))

## ------------------------------------------------------------------------
# Check the labelling was successful
head(mtcars)

## ------------------------------------------------------------------------
# Make the mtcars columns easily available
attach(mtcars)

## ------------------------------------------------------------------------
# Calculate mean for each category of cars
aggregate(mpg~am,data=mtcars,FUN = mean)


## ------------------------------------------------------------------------
# Plot box plots for each category to understand data distribution and identify outliers 
boxplot(mpg~am,main = "Achieved mileage of Automatic and manual cars", xlab = "Type of car (atuomatic/manual)",col = (c("green","blue")), ylab = "Miles per gallon")

## ------------------------------------------------------------------------
#Take the log of mpg data to stabilize variance
log.transformed.mpg = log(mpg)

## ------------------------------------------------------------------------
#Perform a t test assuming equal variance
t.test(log.transformed.mpg~am,var.equal = TRUE)

