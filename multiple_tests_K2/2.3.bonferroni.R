suppressMessages({
  library(dplyr);
  library(readr);
  library(tidyr);
  library(ggplot2);
  library(mvtnorm);
  library(ebnm)
  })


data_path = 'data/'
scenarios_names = c("H0_omega_1",
		    "H0_omega_2",
		    "H0_omega_10", 
		    "H01_omega_1",
		    "H01_omega_2",  
		    "H01_omega_10",
		    "H10_omega_1", 
		    "H10_omega_2",
		    "H10_omega_10",
		    "H11_omega_1",
		    "H11_omega_2",
		    "H11_omega_10",
		    "Hmiss_omega_1",
		    "Hmiss_omega_2",
		    "Hmiss_omega_10")


## read data
### optimal solution + parameters
ALPHA = 0.05
K=2


bonferroni_results = list()
system.time({
for(current_scenario in scenarios_names)
{
  R = 5000
  dec = matrix(nrow= R, ncol = 2)
  for(r in 1:R)
  {
   dat = read.csv(sprintf('%s%s/trial_%s_%i.csv',data_path,current_scenario,current_scenario,r))
   names(dat) = c('Y','S',"K","C")
   ## compute S_k and p-values
   suppressMessages({stats = dat |> group_by(K,C) |> summarize(mean_Y = mean(Y), mean_S = mean(S), var_Y = var(Y)/n(), var_S = var(S)/n())})
  mean_Y =  stats$mean_Y[stats$C==1] -  stats$mean_Y[stats$C==0]
  var_Y =  stats$var_Y[stats$C==1] +  stats$var_Y[stats$C==0]
  mean_S =  stats$mean_S[stats$C==1] -  stats$mean_S[stats$C==0]

  
  p_vals = 1-pnorm(mean_Y/sqrt(var_Y))
  dec[r,] = as.numeric(p_vals <= ALPHA/K)
  }
bonferroni_results[[current_scenario]] = dec
cat(sprintf('done with %s \n', current_scenario))
}
})

save(bonferroni_results, file ="bonferroni_results.rdata")
print("Done!")

