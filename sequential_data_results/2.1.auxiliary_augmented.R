suppressMessages({
  library(dplyr);
  library(readr);
  library(tidyr);
  library(ggplot2);
  library(mvtnorm);
  library(ebnm);
  library(gsDesign)
 library(parallel)
  })


  do_one_sim = function(r,...) 
  {
   dat = read.csv(sprintf('%s%s/trial_%s_%i.csv',data_path,current_scenario,current_scenario,r))
   Z_Y_IA =  Te_to_Zy(compute_Zs(dat[1:N_IA,]))
   Z_Y_FA =  Te_to_Zy(compute_Zs(dat))
   tryCatch({IA_posterior_parameters = approximate_posterior(dat[1:N_IA,], priorValues)}, error = \(e) "" )
   if(!isSymmetric(IA_posterior_parameters$var))
   {
    IA_posterior_parameters$var[!lower.tri(IA_posterior_parameters$var, diag = TRUE)] = 
    t(IA_posterior_parameters$var)[!lower.tri(IA_posterior_parameters$var, diag = TRUE)]
   }

   PP_sim = matrix(0,N_P_SIM,5)
   for( s in 1:N_P_SIM)
   {
   tryCatch({
  PPsample = bind_rows(dat[1:N_IA,],sample_posterior_predictive(IA_posterior_parameters, priorValues,priorValues$N - N_IA))
  
  # compute the Z values
   PP_sim[s,] =  compute_Zs(PPsample)}, error = \(e) return(NA))
}
  

   pred_p = mean(apply(PP_sim,1,\(x) x[1]/sqrt(x[3]) >= C2),na.rm = TRUE)
   if(pred_p<= BETA)  dec  = 'futility-stop'
   else if(Z_Y_IA>=C1) dec  = 'efficacy-stop'
   else if(Z_Y_FA>= C2) dec = 'H1'
   else dec ='H0'
   return(dec)
}


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

source("../R/generate_data_from_prior.R")
source("../R/approximate_posterior_mode.R")
source("../R/sample_posterior_predictive.R")
source("../R/compute_Zs.R")

## extract
Te_to_Zy = \(x) {res = x["Delta_Y"]/sqrt(x["Var_Delta_Y"]); names(res) =NULL;res }


## read data
### optimal solution + parameters
ncpus =8
BETA = 0.13
GAMMA = 2
N_IA = 100
mod = gsDesign(k = 2, test.type = 1, n.fix = 200, alpha = 0.05, beta =0.2, sfupar = GAMMA, endpoint = 'Binomial')
C1     = mod$upper$bound[1]
C2     = mod$upper$bound[2]


A_auxiliary_results = list()
system.time({
for(current_scenario in scenarios_names)
{
  R = 5000
  N_P_SIM = 1000

 cl <- makeCluster(ncpus)
 clusterExport(cl, "do_one_sim", envir = environment())
 clusterExport(cl, "current_scenario", envir = environment())
 clusterExport(cl, "data_path", envir = environment())
 clusterExport(cl, "C1", envir = environment())
 clusterExport(cl, "C2", envir = environment())
 clusterExport(cl, "BETA", envir = environment())
clusterExport(cl,"Te_to_Zy", envir = environment())
clusterExport(cl,"priorValues", envir = environment())
clusterExport(cl,"N_IA", envir = environment())
clusterExport(cl,"N_P_SIM", envir = environment())
clusterEvalQ(cl, lapply(c("../R/generate_data_from_prior.R","../R/approximate_posterior_mode.R", "../R/sample_posterior_predictive.R","../R/compute_Zs.R"), \(x) source(x)))
clusterEvalQ(cl, suppressMessages({
  library(dplyr);
  library(readr);
  library(tidyr);
  library(ggplot2);
  library(mvtnorm);
  library(ebnm);
  library(gsDesign)
 library(parallel)
  }))
decisions = parSapply(cl, 1:R, do_one_sim)
stopCluster(cl)
save(decisions, file = paste0("A_auxiliary_",current_scenario,".rdata"))
cat(sprintf('done with %s \n', current_scenario))
}
})



