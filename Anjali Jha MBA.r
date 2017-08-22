
# Analysis of Airline Ticket Pricing
# NAME: Anjali Jha
# EMAIL: anjalijha167@gmail.com
# COLLEGE / COMPANY: IIT Kharagpur
####Im this file ,I have written all th codes related to the project####
#######################on MBA Students#############################
###The main aim of these lines of code is to analyse the dataset##
##Read the data into R
mba.df<-read.csv(paste("MBA Starting Salaries Data.csv",sep = ""))
View(mba.df)
##Create summary statistics (e.g. mean, standard deviation, median, mode) for##
##the important variables in the dataset.##
library(psych)
summary(mba.df)
describe(mba.df$age)
describe(mba.df$sex)
describe(mba.df$gmat_tot)
describe(mba.df$gmat_qpc)
describe(mba.df$gmat_vpc)
describe(mba.df$gmat_tpc)
describe(mba.df$s_avg)
describe(mba.df$f_avg)
describe(mba.df$quarter)
describe(mba.df$work_yrs)
describe(mba.df$salary)
describe(mba.df$satis)
##The mean of salaries and satisfication is ambiguous because of the 0s, 998s and 999s in salary and the 998s and 999s in satis#
mbap.df<-mba.df[which(mba.df$salary>999),]
mbanp.df<-mba.df[which(mba.df$salary==0),]
#Draw Box Plots / Bar Plots to visualize the distribution of each
#variable independently#
#Boxplot to see Average age of candidates
boxplot(mba.df$age,horizontal=TRUE,xlab="Age of candidates",main ="Average age of candidates")
# Boxplot to see the range of GMAT score of candidates
boxplot(mba.df$gmat_tot,horizontal=TRUE,xlab="Total GMAT score of candidates",main ="Total GMAT score of candidates")
#Boxplot to see quantitative GMAT percentile of candidates
boxplot(mba.df$gmat_qpc,horizontal=TRUE,xlab="Quantitative GMAT percentile of candidates",main ="Quantitative GMAT percentile of candidates")
#Boxplot to see verbal GMAT percentile of candidates
boxplot(mba.df$gmat_vpc,horizontal=TRUE,xlab="Verbal GMAT percentile of candidates",main ="Verbal GMAT percentile of candidates")
# Boxplot to see overall GMAT percentile
boxplot(mba.df$gmat_tpc,horizontal=TRUE,xlab="Overall GMAT percentile of candidates",main ="Overall GMAT percentile of candidates")
# Boxplot to see spring MBA average pf candidates
boxplot(mba.df$s_avg,horizontal=TRUE,xlab="Spring MBA average of candidates",main ="Spring MBA average of candidates")
#Boxplot to see fall MBA average of candidates
boxplot(mba.df$f_avg,horizontal=TRUE,xlab="Fall MBA average of candidates",main ="Fall MBA average of candidates")
#Boxplot to see years of work experience
boxplot(mba.df$work_yrs,horizontal=TRUE,xlab="Years of work experience of candidates",main ="Average year of work experience of candidates")
# Boxplot to see first language (1=English; 2=other) 
boxplot(mba.df$frstlang,horizontal=TRUE,xlab="First language (1=English; 2=other) of candidates",main ="First language of candidates")
#Boxplot to see Average salary
boxplot(mba.df$salary,horizontal=TRUE,xlab="Salary of candidates",main ="Average salary of candidates")
# Boxplot to see degree of satisfaction with MBA program (1= low, 7 = high satisfaction)
boxplot(mba.df$satis,horizontal=TRUE,xlab="Degree of satisfaction with MBA program (1= low, 7 = high satisfaction) of candidates",main ="Average degree of satisfaction of candidates")

##Draw Scatter Plots to understand how are the variables correlated pair-wise##
library(car)
attach(mbap.df)
#scatterplot matrix for salary and sex
scatterplotMatrix(formula=~salary+sex,cex=0.6,diagonal="histogram")
#Scatterplot matrix for salary and age
scatterplotMatrix(formula=~salary+age,cex=0.6,diagonal="histogram")
#Scatterplot matrix for salary and gmat total
scatterplotMatrix(formula=~salary+gmat_tot,cex=0.6,diagonal="histogram")
#Scatterplot matrix for salary and gmat overall
scatterplotMatrix(formula=~salary+gmat_tpc,cex=0.6,diagonal="histogram")
#Scatterplot matrix for salary and spring avg
scatterplotMatrix(formula=~salary+s_avg,cex=0.6,diagonal="histogram")
#Scatterplot matrix for salary and fall avg
scatterplotMatrix(formula=~salary+f_avg,cex=0.6,diagonal="histogram")
#Scatterplot matrix for salary and work experience
scatterplotMatrix(formula=~salary+work_yrs,cex=0.6,diagonal="histogram")
#Scatterplot matrix for salary and first language
scatterplotMatrix(formula=~salary+frstlang,cex=0.6,diagonal="histogram")
plot(work_yrs,salary, col="blue", main="Salary vs Work Experience", xlab="Years", ylab="Salary")

scatterplotMatrix(formula = ~sex + salary, cex = 0.6, diagonal = "histogram")
plot(sex,salary, col="blue", main="Salary vs Sex", xlab="Sex", ylab="Salary")

##Draw a Corrgram##
library(corrgram)
attach(mbap.df)
corrgram(mbap.df,order=TRUE,lower.panel = panel.shade,upper.panel = panel.pie,
         text.panel = panel.txt,main="Corrgram of Placed mbas intercorrelations")
##Create a Variance-Covariance Matrix##
cov(mbap.df)
cor(mbap.df)
# cor.test of price to individual factors#
cor.test(salary,work_yrs)
cor.test(salary, s_avg)
cor.test(salary, frstlang)
cor.test(salary,sex)
cor.test(salary,satis)
##Take a subset of the dataset consisting of only those people who actually got a job.#
job <- subset(mba.df, salary>1000, select = sex:salary)
#Subset of candidates who are not employed
unemployed.sub <- subset(mba.df, salary<1, select = sex:salary)
##Think about the problem as   y = f(x),   
##where y = Starting Salary and x = various factors that it could depend upon##
#Examples:impact of {gender; first language; prior work experience; GMAT performance; MBA performance} 
#etc in determining the Starting Salary #

reg<-lm(salary~sex+gmat_tot+gmat_qpc+gmat_tpc+s_avg+f_avg+work_yrs+frstlang+quarter-1,data=job)
summary(reg)
##Rejecting the insignificant variable##
reg1<-lm(salary~gmat_tot+gmat_tpc+work_yrs-1,data = job)
summary(reg1)
sex#Model 2 gives better r square so considering it
# Positive t value for gmat_tot, first language English and work experience(Higher the positive value, lesser the likelihood of value being 0 by chance)
# The model's, p-value:2e-16 is also lower than the statistical significance level of 0.05, this indicates that we can safely reject the null hypothesis that the value for the coefficient is zero (or in other words, the predictor variable has no explanatory relationship with the response variable)
# The model has a F Statistic of 1.41e+3 on 3 and 100 DF which is moderately high
##Run t-tests, as appropriate##
library(UsingR)
#Null Hypothesis:-There is no relation between gender and salary of the person 
t.test(mba.df$salary~mba.df$sex)
#The p-value is 0.6 which suggests that we cannot reject the null hypothesis##
##Null Hypothesis:-There is no relation between first language and salary of a person## 
t.test(mba.df$salary~mba.df$frstlang)
##The p-value of 0.7 suggests that we cannot rejecthe null hypothesis
##Run chi-square tests, as appropriate##
mytable <- xtabs(~salary, data = job)
chisq.test(mytable)
