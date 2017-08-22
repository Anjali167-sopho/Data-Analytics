###Titanic data set###
##Read the Titanic data set into R.  Create a dataframe called "titanic". #
titanic.df<-read.csv(paste("titanic.csv"))
View(titanic.df)
#Use R to count the total number of passengers on board the Titanic.#
summary(titanic.df)
dim(titanic.df)
#Use R to count the number of passengers who survived the sinking of the Titanic.#
library(vcd)
mytable<-with(titanic.df,table(Survived))
mytable
#Use R to measure the percentage of passengers who survived the sinking of the Titanic.#
prop.table(mytable)
#Use R to count the number of first-class passengers# 
#who survived the sinking of the Titanic.  (Hint: You could use xtabs())#
mytable<-xtabs(~Pclass+Survived,data = titanic.df)
mytable
#Use R to measure the percentage of first-class passengers# 
#who survived the sinking of the Titanic.   (Hint:  You could use  prop.table())#
prop.table(mytable,1)*100
#Use R to count the number of females from First-Class# 
#who survived the sinking of the Titanic#
mytable<-xtabs(~Sex+Survived+Pclass,data=titanic.df)
mytable
#Use R to measure the percentage of survivors who were female#
mytable<-xtabs(~Sex+Survived,data = titanic.df)
mytable
prop.table(mytable)*100
#Use R to measure the percentage of females on board the Titanic who survived#
prop.table(mytable,1)*100
#Run a Pearson's Chi-squared test to test the following hypothesis:#
#Hypothesis:  The proportion of females onboard who survived# 
#the sinking of the Titanic was higher than the proportion of males# 
#onboard who survived the sinking of the Titanic.#
chisq.test(mytable)
#Use R to create a table showing# 
#the average age of the survivors and the average age of the people who died.#
mytable<-aggregate(titanic.df$Age,by=list(Survived=titanic.df$Survived),mean)
mytable
#Use R to run a t-test to test the following hypothesis:#
#H2: The Titanic survivors were younger than the passengers who died.#
t.test(Age~Survived,data = titanic.df,alternative="greater")

