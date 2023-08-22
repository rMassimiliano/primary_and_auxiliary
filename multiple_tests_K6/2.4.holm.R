suppressMessages({
  library(dplyr);
  library(readr);
  library(tidyr);
  library(ggplot2);
  library(mvtnorm);
  library(ebnm)
  })


data_path = 'data/'
scenarios_names = c(
#  "H01_omega_1",
#  "H01_omega_2",
#  "H0_omega_10",
#  "H10_omega_1",
#  "H10_omega_2",
#  "H11_omega_10",
#"H01_omega_10",
# "H0_omega_1",
#   "H0_omega_2",
#   "H10_omega_10",
# "H11_omega_1",
#  "H11_omega_2",
 "Hmiss_omega_1",
 "Hmiss_omega_2",
 "Hmiss_omega_10")


source("../R/distribute_weights.R")



## read data
### optimal solution + parameters
ALPHA = 0.05
K=6


holm_results = list()
system.time({
for(current_scenario in scenarios_names)
{
  R = 5000
  dec = matrix(nrow= R, ncol = K)
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
  p_adj = p.adjust(p_vals, method ='holm')
  dec[r,] = as.numeric(p_adj <= ALPHA)
  }
holm_results[[current_scenario]] = dec
## save intermediate steps in case something goes wrong
#save(A_auxiliary_results, file ="tmp_A_auxiliary_results.rdata")
cat(sprintf('done with %s \n', current_scenario))
}
})

save(holm_results, file ="holm_results.rdata")
print("Done!")

