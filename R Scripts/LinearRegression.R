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
  tab$Oil*tab$Filler
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

# Calculate beta vector
beta<-invProd%*%XT%*%y

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