
# Analysis of Airline Ticket Pricing
# NAME: Anjali Jha
# EMAIL: anjalijha167@gmail.com
# COLLEGE / COMPANY: IIT Kharagpur
####Im this file ,I have written all th codes related to the mini project####
#######################on Airline Industry#############################
###The main aim of these lines of code is to answer the question###
##What factors explain the difference in price##
##between an economy ticket and a premium-economy airline ticket?##
##Read the data into R
air.df<-read.csv(paste("SixAirlines.csv",sep="")) ##Reading the csv file##
View(air.df)
##Summarize the data to understand#
#the mean, median, standard deviation of  each variable#
library(psych)
describe(air.df)
summary(air.df)
##Draw Box Plots / Bar Plots#
##to visualize the distribution of each variable independently##
#boxplots to see the price of economy seats
boxplot(air.df$PRICE_ECONOMY,horizontal=TRUE,xlab="Price of economy seats",main="Average Price of economy seats")
#boxplots to see the price of premium seats
boxplot(air.df$PRICE_PREMIUM,horizontal=TRUE,xlab="Price of premium seats",main="Average Price of permium seats")

#boxplots to see the average no of seats
boxplot(air.df$N,horizontal=TRUE,main="Average No.of seats")

#boxplots to see the price of economy seats in each airline
boxplot(PRICE_ECONOMY~AIRLINE,data=air.df,horizontal=TRUE,xlab="Price of economy seats",ytab="Airline",main="price of economy seats in each airline",las=1)

#boxplots to see the price of premium seats in each airline
boxplot(PRICE_PREMIUM~AIRLINE,data=air.df,horizontal=TRUE,xlab="Price of PREMIUM seats",ytab="Airline",main="price of Premium seats in each airline",las=1)

#boxplots to see the no of seats in each airline
boxplot(N~AIRLINE,data=air.df,horizontal=TRUE,xlab="No.of seats",ytab="Airline",main="No.of seats in each airline seats in each airline",las=1)

#boxplots to see the no of economy seats in each airline
boxplot(SEATS_ECONOMY~AIRLINE,data=air.df,horizontal=TRUE,xlab="No.of seats",ytab="Airline",main="N0.of economy seats in each airline seats in each airline",las=1)

#boxplots to see the no of permium seats in each airline
boxplot(SEATS_PREMIUM~AIRLINE,data=air.df,horizontal=TRUE,xlab="No.of seats",ytab="Airline",main="No.of permium seats in each airline seats in each airline",las=1)

##Draw Scatter Plots to understand how are the variables correlated pair-wise##
library(car)
#Scatter plots for ECONOMY price and economy Seats
scatterplotMatrix(formula=~PRICE_ECONOMY+SEATS_ECONOMY,cex=0.6,data=air.df,diagonal="histogram")

#Scatter plots for permium price and premium Seats
scatterplotMatrix(formula=~PRICE_PREMIUM+SEATS_PREMIUM,cex=0.6,data=air.df,diagonal="histogram")

#Scatter plots for NO OF SEATS
scatterplotMatrix(formula=~N+SEATS_PREMIUM+SEATS_ECONOMY,cex=0.6,data=air.df,diagonal="histogram")

##Draw a Corrgram; Create a Variance-Covariance Matrix##
library(corrgram)
corrgram(air.df,main="Corrogram of all variables",lower.panel = panel.shade, upper.panel = panel.pie, text.panel = panel.txt,order=TRUE)

##Create a Variance-Covariance Matrix##
air1.df<-air.df[,-c(1)]
View(air1.df)
cov(air1.df)
##Articulate a Hypothesis (or two) that you could test using a Regression Model##
#1. Total no.of seats in Boing aircraft is lessthan Airbus aircraft 
t.test(N~AIRCRAFT,data=air.df)

#2. Total no.of economy seats in Boing aircraft is lessthan Airbus aircraft 
t.test(SEATS_ECONOMY~AIRCRAFT,data=air.df)

#3. Total no.of Premium seats in Boing aircraft is lessthan Airbus aircraft 
t.test(SEATS_PREMIUM~AIRCRAFT,data=air.df)

#4. Total COST of Premium seats in Boing aircraft is lessthan Airbus aircraft 
t.test(PRICE_PREMIUM~AIRCRAFT,data=air.df)

#5. Total COST of ECONOMY seats in Boing aircraft is lessthan Airbus aircraft 
t.test(PRICE_ECONOMY~AIRCRAFT,data=air.df)



##Formulate a Regression Model:##
#1.Regression model to estimate the price of economy seat(Y) from predictors FLIGHT_DURATION(X1),SEATS_ECONOMY(X2),PITCH_ECONOMY(X3),WIDTH_ECONOMY(X4),QUALITY(X5),MONTH(X6),AIRCRAFT(X7) and AIRLINE(X8)
#Y=B0+B1*X1+B2*X2+B3*X3+B4*X4+B5*X5+B6*X6+B7*X7+B8*X8
reg1<-lm(PRICE_ECONOMY~FLIGHT_DURATION+SEATS_ECONOMY+PITCH_ECONOMY+WIDTH_ECONOMY+QUALITY+MONTH+AIRCRAFT+AIRLINE,data=air.df)
summary(reg1)

#How relative price is fitted into model
fitted(reg1)
#coefficiants of regression model
reg1$coefficients
#residuals of regression model
reg1$residuals


#2.Regression model to estimate the price of PREMIUM seat(Y) from predictors FLIGHT_DURATION(X1),SEATS_PREMIUM(X2),PITCH_PREMIUM(X3),WIDTH_PREMIUM(X4),QUALITY(X5),MONTH(X6),AIRCRAFT(X7) and AIRLINE(X8)
#Y=B0+B1*X1+B2*X2+B3*X3+B4*X4+B5*X5+B6*X6+B7*X7+B8*X8
reg2<-lm(PRICE_PREMIUM~FLIGHT_DURATION+SEATS_PREMIUM+PITCH_PREMIUM+WIDTH_PREMIUM+QUALITY+MONTH+AIRCRAFT+AIRLINE,data=air.df)
summary(reg2)

#How relative price is fitted into model
fitted(reg2)
#coefficiants of regression model
reg2$coefficients
#residuals of regression model
reg2$residuals





#10
# this regression model(1&2) helps to estimate the price of seat(premium or economy) in any month, of any width, of any pitch, of any aircraft, of any airline, of any quality
# In this regression model by making AIRCRAFT value constant to 0 or 1, It helps to test the hypothesis 4 and hypothesis 5
# means Y=B0+B1*X1+B2*X2+B3*X3+B4*X4+B5*X5+B6*X6+B7*(IT'S VALUE EITHER 0 OR 1)+B8*X8
  
