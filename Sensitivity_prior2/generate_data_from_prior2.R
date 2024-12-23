#' Function that generates trial data from the prior model descibed in section 4 of the main manuscript.


#' Function that generates trial data from the prior model descibed in section 4. We use the same prior for all the groups.
#' No effect on the auxiliary imply no effect on the primary, while if there is an effect on the auxiliary the effect on the primary is `c` times lower than the effect on the auxiliary


#' @param  `param` a list of values that are used to generate the data
# it requires: 1 effect_prob, in the paper is \xi, ex omega  in the code
# re_var: variance of the latent variable Z ex, sigma2_z,
# S_intercept[2]: mean and variance of the intercept of S
# S_effect_slab[2] =  mean and variance of the slope of S, if non zero
# Y_intercept[2] =   mean and var for the intercept of Y
# Y_a, Y_b = parameter for the beta prior in the Y_effect_reduction \in (0,1): how much smaller is the effect on Y compared on the one of S   
# N total sample size
# K number of groups 
# p_k  biomarker proportion


prior_data_generation =function(param)
{

 Nk = with(param,c(rmultinom(1,N,p_k)))
 effectk = numeric(param$K)
 effect2k = numeric(param$K)
 effect_reductionk = 99
 trialData = tibble(C_i = numeric(),K_i = numeric(), S = numeric(),Y = numeric())

 ## use a common effect reduction across groups
 Y_effect_reduction = rbeta(1,param$Y_a, param$Y_b)
 effect_reductionk = Y_effect_reduction
 for(k in 1:param$K)
 {
 ## generate beta_s and beta_y
  beta_s = numeric(2)
  beta_y = numeric(2)
  beta_s[1] = rnorm(1, param$S_intercept[1], sqrt(param$S_intercept[2]))

 ##is there an effect of the drug 
 ## for the spike and slab
 effect = rbinom(1,1, prob = param$effect_prob)
 effect2 = rbinom(1,1, prob = 0.9)
beta_s[2] = ifelse(effect,rnorm(1 , param$S_effect_slab[1], sqrt(param$S_effect_slab[2])),0)
 effectk[k] = effect
 effect2k[k] = effect2

beta_y[1] = rnorm(1, param$Y_intercept[1], sqrt(param$Y_intercept[2]))
beta_y[2] = ifelse(effect*effect2,Y_effect_reduction*beta_s[2] ,0)

 ## generate data from the prior predictive
 K_i = rep(k, Nk[k])
 Z_i = rnorm(Nk[k],0,sqrt(param$re_var))
 C_i = rbinom(Nk[k],1, prob = 0.5)
 p_y = sapply(1:Nk[k], \(i) beta_y[1] + ifelse(C_i[i]==1,beta_y[2],0) + Z_i[i])
 p_s = sapply(1:Nk[k],\(i) beta_s[1] + ifelse(C_i[i]==1,beta_s[2],0) + Z_i[i])
 S = rbinom(Nk[k],1,prob = plogis(p_s))
 Y = rbinom(Nk[k],1,prob = plogis(p_y))

 trialData = trialData |> bind_rows( tibble(C_i ,K_i , S ,Y ) )
 }
 return(list(data = trialData, beta_y = beta_y, beta_s = beta_s, effect = effectk, effect2 = effect2k, residual_Y_effect = effect_reductionk))
 }


