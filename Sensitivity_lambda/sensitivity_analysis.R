suppressMessages({
library(dplyr);
library(readr);
library(tidyr);
library(data.table)
library(ggplot2)
})

source("../R/distribute_weights.R")
source("../R/generate_data_from_prior.R")



alpha = 0.05
beta_seq = seq(0.1,20, l = 100)
K = 2

results = lst()  
for(r in 1:5000)
{
 results[[r]] = fread(sprintf("results/simulation_%i.csv",r)) 
}
results = rbindlist(results)


lambda_seq = seq(0,1, l = 10)
mymean = \(x) mean(x, na.rm = TRUE)
optimal_solutions = tibble(lambda = lambda_seq, beta = numeric(length(lambda_seq)))

for(r in 1:length(lambda_seq))
{
  l  = lambda_seq[r]
  cres = results 
  cres[,c('u'):= list(truePositive - l*falsePositive)]
  cres = cres[, by = beta, c(lapply(.SD, mymean))]
  
  cres[,sim:= NULL]
  b = cres$beta[-NROW(plotData)]
  u =  cres$u[-NROW(plotData)]
  beta_seq2 = seq(0,20,l = 500)
  hat_u = loess(u~b, span = 0.4)
  u_smooth = predict(hat_u, beta_seq2)
  # plot(beta_seq2,u_smooth)
  beta_star =  beta_seq2[which.max(u_smooth)]
  optimal_solutions$beta[r] = beta_star
}

with(optimal_solutions, plot(y = beta, x = lambda))
saveRDS(optimal_solutions, file = 'optimal_solutions.rds')



#######################
results[, c('u05'):=
	list(truePositive - 0.5*falsePositive)]

results[, c('u02'):=
	list(truePositive - 0.2*falsePositive)]

results[, c('u1'):=
	list(truePositive - 1*falsePositive)]

mymean = \(x) mean(x, na.rm = TRUE)
plotData = results[, by = beta, c(lapply(.SD, mymean))]
plotData[,sim:= NULL]

## lambda = 1/2
 b = plotData$beta[-NROW(plotData)]
 u =  plotData$u05[-NROW(plotData)]
 beta_seq2 = seq(0,20,l = 500)
 hat_u = loess(u~b, span = 0.4)
 u_smooth = predict(hat_u, beta_seq2)
# plot(beta_seq2,u_smooth)
 beta_star =  beta_seq2[which.max(u_smooth)]
 plot(u_smooth~beta_seq2, type = 'l'); abline(v = beta_star)


## lambda = 0.2
 b = plotData$beta[-NROW(plotData)]
 u =  plotData$u02[-NROW(plotData)]
 beta_seq2 = seq(0,20,l = 500)
 hat_u = loess(u~b, span = 0.4)
 u_smooth_02 = predict(hat_u, beta_seq2)
# plot(beta_seq2,u_smooth)
 beta_star_02 =  beta_seq2[which.max(u_smooth_02)]
 plot(u_smooth_02~beta_seq2,  type = 'l'); abline(v = beta_star_02)


## lambda = 1
 b = plotData$beta[-NROW(plotData)]
 u =  plotData$u1[-NROW(plotData)]
 beta_seq2 = seq(0,20,l = 500)
 hat_u = loess(u~b, span = 0.4)
 u_smooth_1 = predict(hat_u, beta_seq2)
# plot(beta_seq2,u_smooth)
 beta_star_1 =  beta_seq2[which.max(u_smooth_1)]
 plot(u_smooth_1~beta_seq2, type = 'l'); abline(v = beta_star_1)
