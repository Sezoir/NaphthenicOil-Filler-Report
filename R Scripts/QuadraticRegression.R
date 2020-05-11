# Set working directory
dir<-getwd()
if(!is.null(dir)) setwd(dir) else stop("Working directory is incorrect")

# Load data as a table
tab<-read.table("Viscos.txt", header = TRUE)

# Create X matrix
X<-cbind(
  rep(1, time = nrow(tab)),
  tab$Oil,
  tab$Filler,
  tab$Oil*tab$Filler,
  tab$Oil^2,
  tab$Filler^2
)

# Get/store transpose of X
XT<-t(X)

# Get value of n
n<-nrow(X)

# Times transpose by X
prod<-XT %*% X

# Find the inverse of prod
invProd<-solve(prod)

# Create y response vector
y<-tab$Visc

# Get value of X^Ty
XTy<-XT%*%y

# Calculate beta vector
beta<-invProd%*%XTy

# Calculate SSE
SSE<-t(y)%*%y-t(beta)%*%XT%*%y

# Calculate estimate of variance
var<-SSE/(nrow(X) - ncol(X))

# Calculate the SST
SST<-t(y)%*%y

# Calculate mean of y
my = mean(y)

# Calculate value of SST_C
SST_C<-SST-n*(my^2)

# Calculate coefficient of regression
R<-(SST_C-SSE)/SST_C

# Get diagonal of (X^TX)^-1
dia<-diag(invProd)

# Get values of test statistics
ts<-beta/(sqrt(var)[1]*sqrt(dia))

# Store guessed values from chemist
f_0=c(1, 10, 50, 500, 100, 2500)

# Calculate f_0%*%beta
ciMean<-f_0%*%beta

# Calculate f_0(X^TX)^-1f_0
ciP<-f_0%*%invProd%*%f_0

# 97.5% t distro
ciT<-2.110

# Calculate CI +-
ciPM<-ciT*var*sqrt(ciP)

# Calculate confidence interval as 2 vec
ci<-c(ciMean-ciPM, ciMean+ciPM)

# Calculate PI +-
piPM<-ciT*var*sqrt(1+ciP)

# Calculate predicition interval as 2 vec
pi<-c(ciMean-piPM, ciMean+piPM)


library(ggplot2)
library(plotly)

graph <- plot_ly(tab, x = ~Oil, y = ~Filler, z = ~Visc) %>%
  add_markers()
     #add_trace(x = px, y = py, z = pz, name = 'trace 1', mode = 'lines+markers') 
graph