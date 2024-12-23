suppressMessages({
library(dplyr);
library(readr);
library(tidyr);
library(data.table)
library(ggplot2)
})

cpath = getwd()
setwd("../primary_and_auxiliary/multiple_tests_prior_K2/")

source("../R/distribute_weights.R")
source("../R/generate_data_from_prior.R")



alpha = 0.05
beta_seq = seq(0,20, l = 100)
K = 2

results = lst()  
for(r in 1:5000)
{
 results[[r]] = fread(sprintf("results/simulation_%i.csv",r)) 
}
results = rbindlist(results)


lambda_seq = seq(0,1,0.1)[-1]
optimal_solutions = tibble(lambda = lambda_seq, beta = numeric(10))

mymean = \(x) mean(x, na.rm = TRUE)
beta_seq2 = seq(0,20,l = 500)

for(i in 1:length(lambda_seq))
{
  cres = results 
  cres[, c('u'):= list(truePositive - lambda_seq[i]*falsePositive)]

  cres = cres[, by = beta, c(lapply(.SD, mymean))]
  cres[,sim:= NULL]
  b = cres$beta[-NROW(cres)]
  u =  cres$u[-NROW(cres)]
  hat_u = loess(u~b, span = 0.4)
  u_smooth = predict(hat_u, beta_seq2)
  beta_star =  beta_seq2[which.max(u_smooth)]
  optimal_solutions$beta[i] = beta_star 
}

setwd(cpath)
saveRDS(optimal_solutions, file = "optimal_solutions.rds")


