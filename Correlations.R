# This R file is meant to accompany the discussion on Correlations


## ------------------------------------------------------------------------
states<- state.x77[,1:6] # take a subset of 6 columns
View(states) # view the dataset

## ------------------------------------------------------------------------
states<- state.x77[,1:6] # take a subset of 6 columns
dim(states)  # it has 50 rows, 6 columns
library(psych)
describe(states) # see the Compative Statics -- mean, sd, median 

## ------------------------------------------------------------------------
cov(states)

## ------------------------------------------------------------------------
cor(states)

## ------------------------------------------------------------------------
cor(states, method="spearman")

## ------------------------------------------------------------------------
x <- states[,c("Population", "Income", "Illiteracy", "HS Grad")]
y <- states[,c("Life Exp", "Murder")]
cor(x,y)

## ------------------------------------------------------------------------
cor.test(states[,"Life Exp"], states[,"Murder"])

## ------------------------------------------------------------------------
library(psych)
corr.test(states, use="complete")

## ------------------------------------------------------------------------
describe(mtcars) # Summary statistics of 11 Columns in mtcars

## ------------------------------------------------------------------------
options(digits=2)
cor(mtcars)

## ------------------------------------------------------------------------
library(corrgram)
corrgram(mtcars, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corrgram of mtcars intercorrelations")

## ------------------------------------------------------------------------
corrgram(mtcars, order=TRUE, lower.panel=panel.ellipse,
         upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.minmax,
         main="Corrgram of mtcars data using scatter plots
and ellipses")


## ------------------------------------------------------------------------
cols <- colorRampPalette(c("darkgoldenrod4", "burlywood1",
                           "darkkhaki", "darkgreen"))
corrgram(mtcars, order=TRUE, col.regions=cols,
         lower.panel=panel.shade,
         upper.panel=panel.conf, text.panel=panel.txt,
         main="A Corrgram (or Horse) of a Different Color")

