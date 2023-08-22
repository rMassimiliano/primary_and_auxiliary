suppressMessages(library(tidyverse))
## we generate a multivariate distribution as in our surrogate example
## we then use one the variable to inform the relationship on the other 
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

surrogate_results = list()
system.time({
for(current_scenario in scenarios_names)
{
R = 5000
dec = matrix(nrow= R, ncol = 2)
for(r in 1:R)
{
 dat = read.csv(sprintf('%s%s/trial_%s_%i.csv',data_path,current_scenario,current_scenario,r))
 
## compute summary statistics for the surrogate
suppressMessages({stats = dat |> group_by(K_i,C_i) |> summarize(m=mean(S), s2=var(S)/n())})
mean_bar_S = stats$m[stats$C_i ==1] - stats$m[stats$C_i ==0]
var_bar_S = stats$s2[stats$C_i ==1] + stats$s2[stats$C_i ==0]
Z_S = mean_bar_S/sqrt(var_bar_S)
p_vals = 1-pnorm(Z_S)
## then we use bonferroni
dec[r,] = as.numeric(p_vals <= 0.05/2)
}
surrogate_results[[current_scenario]] = dec
cat(sprintf('done with %s \n', current_scenario))
}
})
save(surrogate_results, file ="surrogate_results.rdata")



