compute_utility_and_save = function(r,...)
{
suppressMessages({
library(dplyr);
library(readr);
library(tidyr);
})

source("../R/distribute_weights.R")



alpha = 0.05
beta_seq = seq(0.1,5, l = 100)
K = 2


#1] load data
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
decisions = matrix(0,nrow = length(beta_seq), ncol =K)
for(b in 1:length(beta_seq))
{
	w = distribute_weights(bar_S, beta_seq[b])
        decisions[b,]  = as.numeric(p_vals <= w*alpha)
}

## for each beta false positive and false negatives
results  = tibble(sim = r, beta = beta_seq, 
truePositive = apply(decisions,1, \(x) sum(x * trialData$effect*trialData$effect2)),
falsePositive = apply(decisions,1, \(x) sum(x * (1-trialData$effect*trialData$effect2))))

## save results
write_csv(results, file = sprintf("results/simulation_%i.csv",r))
}



## parallel computation
## 
N_SIM = 5000
suppressMessages(require(parallel))
ncpus = 7
cl <- makeCluster(ncpus)
clusterExport(cl, "compute_utility_and_save")
system.time({parSapplyLB(cl, 1:N_SIM, compute_utility_and_save)})
stopCluster(cl)
