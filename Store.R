####Managing Employee Retention####

store.df <- read.csv(paste("Store24 (A).csv",sep=""))
View(store.df)
summary(store.df)
library(psych)
describe(store.df)
#Use R to measure the mean and standard deviation of Profit.#
mean(store.df$Profit)
sd(store.df$Profit)
#Use R to measure the mean and standard deviation of MTenure.#
mean(store.df$MTenure)
sd(store.df$MTenure)
#Use R to measure the mean and standard deviation of CTenure.#
mean(store.df$CTenure)
sd(store.df$CTenure)
#In this TASK, we will learn how to sort a dataframe based on a data column#
attach(mtcars)
View(mtcars)
newdata<- mtcars[order(mpg),]   #sort by mpg (ascending)
View(newdata)
newdata[1:5,] #see the first 5 rows
newdata<-mtcars[order(-mpg),]  #sort by mpg (descending)
View(newdata)
detach(mtcars)
#Use R to print the {StoreID, Sales, Profit, MTenure, CTenure} of# 
#the top 10 most profitable stores#
newdata<-store.df[order(-store.df$Profit),1:5]
newdata[1:10,]
#Use R to print the {StoreID, Sales, Profit, MTenure, CTenure} of# 
#the bottom 10 least profitable stores#
newdata<-store.df[order(store.df$Profit),1:5]
newdata[1:10,]
#Use R to draw a scatter plot of Profit vs. MTenure.#
library(car)
scatterplot(store.df$MTenure,store.df$Profit,main="Scatterplot Of Profit vs. Mtenure",xlab="MTenure",ylab="Profit")
par(mfrow=c(1,1))
#Use R to draw a scatter plot of Profit vs. CTenure.#
library(car)
scatterplot(store.df$CTenure,store.df$Profit,main="Scatterplot Of Profit vs. Mtenure",xlab="CTenure",ylab="Profit")
#Use R to construct a Correlation Matrix for all the variables in the dataset#
cor(store.df)
#Use R to measure the correlation between Profit and MTenure#
cor(store.df$Profit,store.df$MTenure)
#Use R to measure the correlation between Profit and CTenure#
cor(store.df$Profit,store.df$CTenure)
#Use R to construct the following Corrgram based on all variables in the dataset#
library(corrgram)
corrgram(store.df,order = TRUE,lower.panel = panel.shade,upper.panel = panel.pie,text.panel = panel.txt,main="Corrgram of Store Variables")
#Run a Pearson's Correlation test on the correlation between Profit and MTenure.#
#What is the p-value?#
cor.test(store.df$Profit,store.df$MTenure,method = "pearson")
#Run a Pearson's Correlation test on the correlation between Profit and CTenure.# 
#What is the p-value?#
cor.test(store.df$Profit,store.df$CTenure,method = "pearson")
#Run a regression of Profit on {MTenure, CTenure Comp, Pop, PedCount, Res, Hours24, Visibility}#
modelProfit=lm(Profit~MTenure+CTenure+Comp+Pop+PedCount+Res+Hours24+Visibility,data = store.df)
modelProfit
