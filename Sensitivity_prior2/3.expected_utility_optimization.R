suppressMessages({
library(dplyr);
library(readr);
library(tidyr);
library(data.table)
library(ggplot2)
})

source("../R/distribute_weights.R")



alpha = 0.05
beta_seq = seq(0.1,20, l = 100)
K = 2

results = lst()  
for(r in 1:5000)
{
 results[[r]] = fread(sprintf("results/simulation_%i.csv",r)) 
}
results = rbindlist(results)

results[, c('u05'):=
	list(truePositive - 0.5*falsePositive)]

mymean = \(x) mean(x, na.rm = TRUE)
plotData = results[, by = beta, c(lapply(.SD, mymean))]
plotData[,sim:= NULL]


 b = plotData$beta
 u =  plotData$u05
 beta_seq2 = seq(0,5,l = 500)
 hat_u = loess(u~b, span = 0.4)
 u_smooth = predict(hat_u, beta_seq2)
 beta_star =  beta_seq2[which.max(u_smooth)]




 plotData |>
   ggplot() +
   geom_point(aes(x = beta, y = u05))+
   geom_line(aes(x= beta_seq2, y = u_smooth), data =tibble(beta_seq2 = beta_seq2, u_smooth = u_smooth),col ='#00249c')+
   geom_vline(xintercept = beta_star, lty = 'dashed') +
   geom_hline(yintercept = u_smooth[which.max(u_smooth)], lty = 'dashed') +
   xlim(0,5)+
   labs(x = 'beta', y ='Utility') +
   theme_classic(11)
   ggsave("optimal_solution_k2_prior_sensitivity.png", width = 7, height =  5.71)
