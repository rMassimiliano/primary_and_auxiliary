cat("Compiling stan models....")
library(rstan)
mod1 = stan_model("stan_code/joint_model_interim_eff.stan", save_dso =  TRUE,auto_write       = TRUE)

#mod2 = stan_model("stan_code/marginal_model_interim.stan", save_dso =  TRUE,auto_write    = TRUE)

#mod3 = stan_model("stan_code/marginal_model_interim_hypoexp.stan", save_dso =  TRUE,auto_write    = TRUE)


cat("DONE\n")
