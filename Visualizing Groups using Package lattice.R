###################################
# 5-CF  ### COMPARING GROUPS: TABLES AND VISUALIZATION ###
############
# Read the data


#1 Load the Cable TV subscription dataset
# ==========
seg.df <- read.csv(paste("5-CableTVSubscribersData.csv", sep=""))
#2 Review the CRM dataset
# ==========
View(seg.df)
str(seg.df)
# Summary Statistics of the  Cable TV subscription dataset
# ==========
summary(seg.df)
library(psych)
describe(seg.df)
attach(seg.df)

# Visualization of a Discrete Variable: Counts by Group
# ==========
library(lattice)
histogram(~subscribe | Segment, data=seg.df)
#8b A better Histogram
# ==========
histogram(~subscribe | Segment, data=seg.df, 
          type="count", 
          layout=c(4,1), 
          col=c("burlywood", "darkolivegreen"))
# Histograms by 2 factors
# ==========
# histogram of counts
histogram(~subscribe | Segment + ownHome, data=seg.df,
          type="count", 
          layout=c(4,2), 
          col=c("burlywood", "darkolivegreen"))
# histogram of percentages
histogram(~subscribe | Segment + ownHome, data=seg.df,
          #          type="count", 
          layout=c(4,2), 
          col=c("burlywood", "darkolivegreen"))
# Continuous Data: "Spreadsheet" style
# ==========
seg.mean <- aggregate(income ~ Segment, data=seg.df, mean)
seg.mean
barchart(income ~ Segment, data=seg.mean, col="grey")

# Continuous data by two factors
# ==========
# split it by home ownership
seg.agg <- aggregate(income ~ Segment + ownHome, data=seg.df, mean)
seg.agg
barchart(income ~ Segment, data=seg.agg, 
         groups=ownHome, auto.key=TRUE,
         par.settings = simpleTheme(col=c("gray95", "gray50")) )

# Boxplots - Continuous Data: "Statistics" style
# ==========
library(lattice)
bwplot(Segment ~ income, data=seg.df, horizontal=TRUE, 
       xlab = "Income")

# Boxplots with two way grouping
# ==========
bwplot(Segment ~ income | ownHome, data=seg.df, 
       horizontal=TRUE, xlab="Income")


