
generate_and_save = function(r,...)
{
	set.seed(10 + r)
   ## load libraries and functions
   suppressMessages({
   library(dplyr);
   library(readr);
   library(tidyr);
   })
   source("../R/distribute_weights.R")
   source("generate_data_from_prior2.R")
   
   ## set parameters to generate the data
   ## same prior for the K groups

   priorValues = lst( 
     S_intercept = c(-0.8,0.5),
     effect_prob = 0.10,
     S_effect_slab = c(0,0.8),
     Y_intercept = c(-1.5,0.5),
     Y_effect_reduction = 0.8,
     re_var      = 1,
     N = 200,
     K = 2,
     p_k = c(0.6,0.4),
     Y_a = 6,
     Y_b = 1)
   
   ## simulate data from the prior
   trialData=  prior_data_generation(priorValues)
  save(trialData,file = sprintf("data/prior_simulation_%i.rdata",r))
  if(r%%100){ print(sprintf("simulated and saved dataset %i",r))}
}





## parallel computation
## 
N_SIM = 5000
suppressMessages(require(parallel))
ncpus = 8
cl <- makeCluster(ncpus)
clusterExport(cl, "generate_and_save")
system.time({parSapply(cl, 1:N_SIM, generate_and_save)})
stopCluster(cl)
