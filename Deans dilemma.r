##This script contains all the codes for week1 day 6 assingment.It is related to a case of Harvard Buisness Case 
#### named "A DEAN’S DILEMMA: SELECTION OF STUDENTS FOR THE MBA PROGRAM" by
##DHIMANT GANATRA  and  U. DINESH KUMAR###
mba.df<-read.csv(paste("Data - Deans Dilemma.csv", sep = ""))
View(mba.df)
summary(mba.df)
library(psych)
describe(mba.df)
# Use R to calculate the median salary of all the students in the data sample #
describe(mba.df$Salary)
summary(mba.df$Salary)
#240000 is the median salary of all the students#
#Use R to calculate the percentage of students who were placed, correct to 2 decimal#
library(vcd)
mytable<-with(mba.df,table(Placement_B))
mytable
round(prop.table(mytable)*100,2)
#79.80% of students were placed#
#Use R to create a dataframe called placed,#
#that contains a subset of only those students who were successfully placed.#
placed.df<-mba.df[which(mba.df$Placement_B>0),]
placed.df
View(placed.df)
#Use R to find the median salary of students who were placed.#
median(placed.df$Salary)
#260000 is the median of the salary of students who were placed#
#Use R to create a table showing the mean salary of males and females, who were placed.#
library(vcd)
gendersal<-aggregate(placed.df$Salary,by=list(gender=placed.df$Gender),mean)
gendersal
#Use R to generate the following histogram showing a#
#breakup of the MBA performance of the students who were placed#
hist(placed.df$Percent_MBA,main = "MBA Perfomance of Placed Students" ,xlab ="MBA Percentage",
     ylab="Count",xlim=c(50,80),ylim=c(0,150),breaks = 3)
#Create a dataframe called notplaced, that contains a subset of #
#only those students who were NOT placed after their MBA.#
notplaced.df<-mba.df[which(mba.df$Placement_B==0),]
View(notplaced.df)
#Draw two histograms side-by-side, #
#visually comparing the MBA performance of Placed and Not Placed students#
par(mfrow=c(1,2))
with(placed.df,hist(placed.df$Percent_MBA,main = "MBA Perfomance Of Placed Students",xlab = "MBA Percentage",ylab = "Count",
                    xlim=c(50,80),ylim=c(0,150),breaks = 3))
with(notplaced.df,hist(notplaced.df$Percent_MBA,main = "MBA Perfomance of not placed students",
                       xlab = "MBA Percentage",ylab = "Count",
                       xlim = c(50,80),ylim = c(0,150),breaks = 3))
par(mfrow=c(1,1))
#Use R to draw two boxplots, one below the other,#
# comparing the distribution of salaries of males and females who were placed,#
boxplot(placed.df$Salary~placed.df$Gender,main="Comparison Of salaries of male and female",
        xlab="salary",ylab="Gender",horizontal=TRUE)
par(mfrow=c(1,1))
#Create a dataframe called placedET,#
#representing students who were placed "after the MBA#
#and who also gave some MBA entrance test before admission into the MBA program#
placedEt.df<-placed.df[which(placed.df$S.TEST>0),]
placedEt.df
View(placedEt.df)
#Draw a Scatter Plot Matrix for#
# 3 variables -- {Salary, Percent_MBA, Percentile_ET} using the dataframe placedET.#
library(car)
scatterplotMatrix(formula=~Salary+Percent_MBA+Percentile_ET,data=placedEt.df,
                  main="Scatter Plot Matrix")
#Use R to create a table showing the average salary of males and females,# 
#who were placed. Review whether there is a gender gap in the data.# 
# In other words, observe whether the average salaries of males is higher# 
#than the average salaries of females in this dataset.#
mytable<-aggregate(placed.df$Salary,by=list(Gender=placed.df$Gender),mean)
mytable
t.test(Salary~Gender,data = placed.df,alternative="less")
