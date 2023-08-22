#### data generating function
bernoulli_copula = function(py,ps, omega)
{ 
    pijk = matrix(0,2,2)
    if(omega == 1)
    {
     ## independent
     pijk[2,2] = py*ps
     pijk[1,1] = (1-py)*(1-ps)
     pijk[2,1] = py*(1-ps)
     pijk[1,2] = (1-py)*ps
    }
    else
    {
      pijk = matrix(0,2,2)
      pijk[2,2] =  (1+ (omega-1)*(ps + py) - sqrt( (1 + (omega-1)*(py+ps))^2 - 4*omega * (omega-1)* py*ps))/(2*(omega-1))
      pijk[2,1] = py - pijk[2,2]
      pijk[1,2] = ps - pijk[2,2]
      pijk[1,1] = (1-py) - pijk[1,2]
    }
    
    return(pijk)
}
###################################

suppressMessages({
  library(dplyr);
  library(readr);
  library(tidyr);
  library(ggplot2);
  library(mvtnorm);
  library(ebnm)
  })


NDATA = 5000
data_path = 'data/'
scenarios_names = c(
		   # "H01_omega_1",
		   # "H01_omega_2",
		   # "H0_omega_10",
		   # "H10_omega_1",
		   # "H10_omega_2",
		   # "H11_omega_10",
		   # "H01_omega_10",
		   # "H0_omega_1",
		   # "H0_omega_2", 
		   # "H10_omega_10",
		   # "H11_omega_1",
		   # "H11_omega_2",
		    "Hmiss_omega_1",
		    "Hmiss_omega_2", 
		    "Hmiss_omega_10")

## define the marginal distribution
true_parameters = lst()
true_parameters$N = 600
## number of groups
true_parameters$K = 6
## biomarker probability vector
true_parameters$p_k = c(0.25,rep(0.15,5))        ## biomarker proportion
## marginals for the primary under the null
true_parameters$pi_ykc = array(0, dim = c(true_parameters$K, 2))
## SOC
true_parameters$pi_ykc[,1] = rep(0.20,true_parameters$K)
## no TE
true_parameters$pi_ykc[,2] = rep(0.20,true_parameters$K)

## marginals for the auxiliary
true_parameters$pi_skc = array(0, dim = c(true_parameters$K, 2))
## auxiliary SOC
true_parameters$pi_skc[,1] = rep(0.50,true_parameters$K)
## auxiliary no TE
true_parameters$pi_skc[,2] = rep(0.5, true_parameters$K)


true_parameters_NULL = true_parameters


system.time({
for(current_scenario in scenarios_names)
{
 ##1 set correct parameters
 H_settings = strsplit(current_scenario,"_omega_")[[1]][1]
if(H_settings == 'H0')
{
 true_parameters = true_parameters_NULL
}
else if(H_settings == 'H01')
{
 ## effect only on auxiliary k=1
 true_parameters = true_parameters_NULL
 true_parameters$pi_skc[1,2]  = 0.75
}
else if(H_settings == 'H10')
{
 ## effect only on primary k=1
 true_parameters = true_parameters_NULL
 true_parameters$pi_ykc[1,2]  = 0.4

}
else if(H_settings == 'H11')
{
 ## effect on both primary and auxiliary k=1
 true_parameters = true_parameters_NULL
 true_parameters$pi_skc[1,2]  = 0.75
 true_parameters$pi_ykc[1,2]  = 0.4
}
else if(H_settings == 'Hmiss')
{


 true_parameters = true_parameters_NULL
 ## positive effect for primary
 true_parameters$pi_ykc[1,2]  = 0.4
 ## negative effect for auxiliary
 true_parameters$pi_skc[1,1]  = 0.75
}

## set the proper correlation
true_parameters$rho = as.numeric(strsplit(current_scenario,"omega_")[[1]][2])

## joint distribution of the outcomes
true_parameters$pi_ys_kc =  array(0, dim = c(2,2,true_parameters$K, 2))
for(k in 1:true_parameters$K)
{
 for(c in 1:2)
 { 
  true_parameters$pi_ys_kc[,,k,c] = bernoulli_copula(true_parameters$pi_ykc[k,c],true_parameters$pi_skc[k,c], omega = true_parameters$rho)
 }
}
pp = true_parameters$pi_ys_kc[,,k,c]
#cov2cor(diag(diag(pp),2)  - pp %*% t(pp))

## generate data
possible_outcomes = list(c(0,0),c(1,0), c(0,1), c(1,1))

for(r in 1:NDATA)
{
trialData = data.frame(Y = numeric(true_parameters$N), S = numeric(true_parameters$N), K_i = numeric(true_parameters$N), C_i = numeric(true_parameters$N))
 for(i in 1:true_parameters$N)
 {
   ## treatment effect 0,1
   Ki      = sample(1:true_parameters$K, size = 1, prob = true_parameters$p_k)
   Ci      = rbinom(1,1,0.5) 
   outcomes =  unlist(sample(possible_outcomes, size = 1, prob = c(true_parameters$pi_ys_kc[,,Ki,Ci + 1])))
   trialData[i,] = cbind(Y = outcomes[1], S =  outcomes[2], K_i = Ki, C_i = Ci)
}	


data.table::fwrite(trialData,
		   file = sprintf("data/%s/trial_%s_omega_%i_%i.csv", current_scenario,H_settings,true_parameters$rho, r))
if(r%%1000 ==0) cat(sprintf("Dataset %i \n",r))
}
cat(sprintf('done with %s \n', current_scenario))
}
})

print("done")
