# Set working directory
dir<-getwd()
if(!is.null(dir)) setwd(dir) else stop("Working directory is incorrect")

# Load data as a table
tab<-read.table("Viscos.txt", header = TRUE)

# Fit the model as a linear model
model.fit<-lm(tab$Visc ~ tab$Oil + tab$Filler + I(tab$Oil*tab$Filler))

# Get summary of model
summa<-summary(model.fit)
summa

# Get anova of model
anov<-anova(model.fit)
anov

# Find variance of response
var<-anov[]$`Mean Sq`[4]
var