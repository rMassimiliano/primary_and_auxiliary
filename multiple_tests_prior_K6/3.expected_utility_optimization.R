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
K = 6

results = lst()  
for(r in 1:5000)
{
 results[[r]] = fread(sprintf("results/simulation_%i.csv",r)) 
}
results = rbindlist(results)

results[, c('u10'):= list(truePositive - 1.0*falsePositive)]

mymean = \(x) mean(x, na.rm = TRUE)
plotData = results[, by = beta, c(lapply(.SD, mymean))]
plotData[,sim:= NULL]


b = plotData$beta[-NROW(plotData)]
u =  plotData$u10[-NROW(plotData)]
beta_seq2 = seq(1,20,l = 500)
hat_u = loess(u~b, span = 0.4)
u_smooth = predict(hat_u, beta_seq2)
#plot(beta_seq2,u_smooth)
beta_star =  beta_seq2[which.max(u_smooth)]

###########################################
###########################################
############ FDR and TPR ##################
###########################################
###########################################


opt_results = tibble()
bonf_results = tibble()
for(r in 1:5000)
{
load(sprintf("data/prior_simulation_%i.rdata",r))
## compute statistics for decision
suppressMessages({
stat  = trialData$data |> group_by(C_i,K_i) |> summarize(
	mean_Y = mean(Y), 
	mean_S = mean(S),
	var_Y  = var(Y)/n(),
	var_S  = var(S)/n(),
	cov_S  = cov(Y,S)/n()
	)
})


bar_Y = stat$mean_Y[stat$C_i == 1] - stat$mean_Y[stat$C_i == 0]
bar_S = stat$mean_S[stat$C_i == 1] - stat$mean_S[stat$C_i == 0]
var_bar_Y = stat$var_Y[stat$C_i == 1] + stat$var_Y[stat$C_i == 0]
p_vals =  1-pnorm(bar_Y/sqrt(var_bar_Y))

## compute decision for each beta in the grid

w = distribute_weights(bar_S, rep(beta_star,K))
decisions  = as.numeric(p_vals <= w*alpha)
opt_results  =  opt_results |> bind_rows(tibble(sim = r,
						beta = beta_star,
						decision1 = decisions[1],
						decision2 = decisions[2],
						decision3 = decisions[3],
						decision4 = decisions[4],
						decision5 = decisions[5],
						decision6 = decisions[6],
						eff1 = trialData$effect[1],
						eff2 = trialData$effect[2],
						eff3 = trialData$effect[3],
						eff4 = trialData$effect[4],
						eff5 = trialData$effect[5],
						eff6 = trialData$effect[6]))

decisions  = as.numeric(p_vals <= alpha/K)
bonf_results = bonf_results |> bind_rows(tibble(sim = r,
						beta = 0,
						decision1 = decisions[1],
						decision2 = decisions[2],
						decision3 = decisions[3],
						decision4 = decisions[4],
						decision5 = decisions[5],
						decision6 = decisions[6],
						eff1 = trialData$effect[1],
						eff2 = trialData$effect[2],
						eff3 = trialData$effect[3],
						eff4 = trialData$effect[4],
						eff5 = trialData$effect[5],
						eff6 = trialData$effect[6]))

if(r%%100==0) cat(sprintf('done with iterations %i \n',r))
}



FDR_aux = mean(with(opt_results,
     (decision1*(1-eff1) +
      decision2*(1-eff2) +
      decision3*(1-eff3) +
      decision4*(1-eff4) +
      decision5*(1-eff5) +
      decision6*(1-eff6) 
      )/(decision1 +
         decision2 +
         decision3 +
         decision4 +
         decision5 +
         decision6 +
	 0.01)),na.rm = TRUE)

FDR_bonf = mean(with(bonf_results,
     (decision1*(1-eff1) +
      decision2*(1-eff2) +
      decision3*(1-eff3) +
      decision4*(1-eff4) +
      decision5*(1-eff5) +
      decision6*(1-eff6) 
      )/(decision1 +
         decision2 +
         decision3 +
         decision4 +
         decision5 +
         decision6 +
	 0.01)),na.rm = TRUE)
## TDR
TDR_aux = mean(with(opt_results,
     (decision1*(eff1) +
      decision2*(eff2) +
      decision3*(eff3) +
      decision4*(eff4) +
      decision5*(eff5) +
      decision6*(eff6) 
      )/(decision1 +
         decision2 +
         decision3 +
         decision4 +
         decision5 +
         decision6 +
	 0.01)),na.rm = TRUE)

TDR_bonf = mean(with(bonf_results,
     (decision1*(eff1) +
      decision2*(eff2) +
      decision3*(eff3) +
      decision4*(eff4) +
      decision5*(eff5) +
      decision6*(eff6) 
      )/(decision1 +
         decision2 +
         decision3 +
         decision4 +
         decision5 +
         decision6 +
	 0.01)),na.rm = TRUE)




###########################################
###########################################
############ PLOT #########################
###########################################
###########################################
text1 = sprintf("(TDR = %.3f, FDR = %.3f)", FDR_aux,TDR_aux)
text2 = sprintf("(TDR = %.3f, FDR = %.3f)", FDR_bonf,TDR_bonf)

plotData |>
ggplot() +
geom_point(aes(x = beta, y = u10))+
geom_smooth(aes(x = beta, y = u10), span = 0.4, se = FALSE, col ='#00249c')+
geom_text(x = 14.9, y = 0.0059, label = text1, col = '#00249c')+
geom_text(x = 14.9, y = 0.0017, label = text2)+
geom_vline(xintercept = beta_star, lty = 'dashed') +
geom_hline(yintercept = u_smooth[which.max(u_smooth)], lty = 'dashed') +
geom_hline(yintercept = plotData[is.na(beta),u10]) +
xlim(0,20)+
labs(x = 'beta', y ='Utility') +
theme_classic(11)
ggsave("optimal_solution_k6.png", width = 7, height =  5.71)







