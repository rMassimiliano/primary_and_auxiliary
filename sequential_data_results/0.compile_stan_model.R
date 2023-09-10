cat("Compliling stan models....")
library(rstan)
mod1 = stan_model("clean_code_stan/joint_model_interim.stan", save_dso =  TRUE,auto_write       = TRUE)

mod2 = stan_model("clean_code_stan/marginal_model_interim.stan", save_dso =  TRUE,auto_write    = TRUE)

cat("DONE\n")
