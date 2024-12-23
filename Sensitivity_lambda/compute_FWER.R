


library(dplyr)
load("A_auxiliary_results_sensitivity.rdata")

FWER = function(x)
{
 mean(rowSums(x)>=1)
}


## Scenarios 1 & 2
fwers = sapply(A_auxiliary_results[c(1:6)],  \(x) apply(x, 3,FWER))
arrayInd(which.max(fwers), .dim = dim(fwers))
summary(c(fwers))
mean(c(fwers)) +c(-2,2) *sd(c(fwers)) 

