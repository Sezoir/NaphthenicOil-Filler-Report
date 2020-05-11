# Delete previous variables
rm(list=ls())

# Set working directory
dir<-getwd()
if(!is.null(dir)) setwd(dir) else stop("Working directory is incorrect")

# Load data as a table
tab<-read.table("Viscos.txt", header = TRUE)

# Store variables from table
Visc<-tab$Visc
Oil<-tab$Oil
Filler<-tab$Filler
Interaction<-tab$Oil*tab$Filler
OilSquared<-tab$Oil^2
FillerSquared<-tab$Filler^2

# Fit the model as a quadratic model
model.fit<-lm(Visc ~ Oil + Filler + Interaction + OilSquared + FillerSquared)

# Get summary of model
summa<-summary(model.fit)
summa

# Get anova of model
anov<-anova(model.fit)
anov

# Find variance of response
var<-anov[]$`Mean Sq`[6]
var

newdata<-data.frame(
  Oil=10,
  Filler=50,
  Interaction = 500,
  OilSquared = 100,
  FillerSquared = 2500
) 

# Find the confidence interval
ci<-predict(model.fit, newdata, interval="confidence", level=0.95) 
ci

# Find the predicition interval
pi<-predict(model.fit, newdata, interval="prediction", level=0.95) 
pi