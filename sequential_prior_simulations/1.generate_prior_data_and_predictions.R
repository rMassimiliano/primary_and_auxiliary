library(dplyr)
library(tidyr)

source("../R/generate_data_from_prior.R")
source("../R/approximate_posterior_mode.R")
source("../R/sample_posterior_predictive.R")
source("../R/compute_Zs.R")


generate_and_save = function(r,...)
{
 library(dplyr)
 library(tidyr)
  
 source("../R/generate_data_from_prior.R")
 source("../R/approximate_posterior_mode.R")
 source("../R/sample_posterior_predictive.R")
 source("../R/compute_Zs.R")

priorValues = lst( 
     S_intercept = c(-0.8,0.5),
     effect_prob = 0.10,
     S_effect_slab = c(0,0.8),
     Y_intercept = c(-1.5,0.5),
     Y_effect_reduction = 0.8,
     re_var      = 1,
     N = 200,
     K = 1,
     p_k = 1,
     Y_a = 6,
     Y_b = 1,
     b_a = 6,
     b_b = 1)




N_IA = priorValues$N/2

tryCatch({
## generate trial
priorSim = prior_data_generation(priorValues)
## compute interim posterior
IA_posterior_parameters = approximate_posterior(priorSim$data[1:N_IA,], priorValues)
## compute Z-values at the end of the trial
R = 1000
PP_sim = matrix(0,R,5)
for( i in 1:R)
{
  PPsample = bind_rows(priorSim$data[1:N_IA,],sample_posterior_predictive(IA_posterior_parameters, priorValues,priorValues$N - N_IA))
  
  # compute the Z values
  PP_sim[i,] =  compute_Zs(PPsample)
}

results = list(priorSim = priorSim, IA_posterior_paramters = IA_posterior_parameters, PP_sim = PP_sim)

save(results,file = sprintf("results/prior_simulation_%i.rdata",r))
print(r)}, error = function(e) print(sprintf("simulation %i gava an error",r)))
}

## parallel computation
## 
N_SIM = 1000
suppressMessages(require(parallel))
ncpus = 6
cl <- makeCluster(ncpus, outfile ='')
clusterExport(cl, "generate_and_save")

boot_LRT = parSapply(cl, 1:N_SIM, generate_and_save)
stopCluster(cl)


