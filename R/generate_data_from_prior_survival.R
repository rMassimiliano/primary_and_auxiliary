#' Function that generates trial data from the prior model descibed in section XX

#' @description This prior uses the model described in Broglio and Berry (2019) leveraging uses the decomposition OS = PFS + SPP. In this implementation we assume: 1] both PFS and SPP are distributed as a exponential random variable; 2]  no effect on the auxiliary (PFS) imply no effect on the primary (OS), 3] SPP has the same distribution for treated and non treated patients, i.e. PFS is a full mediator of the treatment on the primary; 4] accrual time is uniformly distributed with m_patients enrolled for each month; 5] the trial stops end_fup months after the last patients have been enrolled in the trial. 


#' @references  Kristine R. Broglio, Donald A. Berry, Detecting an Overall Survival Benefit that Is Derived From Progression-Free Survival, JNCI: Journal of the National Cancer Institute, Volume 101, Issue 23, 2 December 2009, Pages 1642–1649, https://doi.org/10.1093/jnci/djp369

#' @param  `param` a list of values (i.e., models parameter) that are used to generate the data
# it requires: 1 effect_prob, in the paper is \xi, ex omega  in the code

# S_intercept[2]: mean and variance of the Gaussian prior for intercept of S (i.e., baseline log Hazard) 
# S_effect_slab[2] =  mean and variance of the Gaussian prior the log(HR) of S, if non zero (i.e., the slab)
#effect_prob probability that there is an effect on S
# SPP_log_mean[2] =   mean and var for the log mean (- log rate) of SPP
# N total sample size
# m_patients number of patients enrolled for each 30days
#p_pfs probability of censoring pfs
#p_spp probability of censoring pfs

prior_data_generation_surival =function(param)
{
  beta_s = numeric(2) ## coefficient for S
  beta_p = numeric(1) ## coefficient for SPP
  beta_s[1] = rnorm(1, param$S_intercept[1], sqrt(param$S_intercept[2]))

  ##++++++++++++++++++++++++++++++++++++++++++
  ##++++++++++++++++++++++++++++++++++++++++++
  ##++ generate model parameters +++++++++++++
  ##++++++++++++++++++++++++++++++++++++++++++
  ##++++++++++++++++++++++++++++++++++++++++++
  ## for the spike and slab
  ##is there an effect of the drug 
  effect = rbinom(1,1, prob = param$effect_prob)
  beta_s[2] = ifelse(effect,rnorm(1 , param$S_effect_slab[1], sqrt(param$S_effect_slab[2])),0)
  beta_p = rnorm(1, param$SPP_log_mean[1], sqrt(param$SPP_log_mean[2]))

  ##++++++++++++++++++++++++++++++++++++++++++ 
  ##++++++++++++++++++++++++++++++++++++++++++ 
  ## generate data from the prior predictive
  ##++++++++++++++++++++++++++++++++++++++++++ 
  ##++++++++++++++++++++++++++++++++++++++++++ 
  dat = data.frame(patID =1:param$N ,pfs = numeric(param$N), pfs_status = numeric(param$N), os = numeric(param$N), os_status =  numeric(param$N))

  bounds_ind = 0:(param$N-1) %/% param$m_patients +1
  maxPeriod = max(bounds_ind)
  bounds = cbind(seq(0,30*maxPeriod, by = 30)[1:maxPeriod], seq(30,30*maxPeriod, by = 30)[1:maxPeriod])

  etim = sort(runif(param$N,bounds[bounds_ind,1],bounds[bounds_ind,2]))
  ## equal allocation 
  treat = rbinom(param$N,1,0.5)
  pfs =  rexp(param$N, rate = exp(-beta_s[1] - beta_s[2]*treat))
  spp = rexp(param$N, rate = exp(-beta_p))

 ## we need to apply censoring
  #1] PFS censoring
  C_pfs = rexp(param$N, rate = exp(-beta_s[1] - beta_s[2]*treat) * (param$p_pfs)/(1-param$p_pfs))
  dat$pfs = pmin(pfs,C_pfs)
  dat$pfs_status = 1*I(pfs <= C_pfs) 

  #2] censoring spp
  C_spp = rexp(param$N, rate = exp(-beta_p) * (param$p_spp)/(1-param$p_spp))
  d_spp = pmin(spp,C_spp)
  c_spp = 1 * I(spp <= C_spp)

  ## case for os
   # censored PFS -> OS = PFS, OS_status = 0 
  ## OS is censored ar PFS
  dat$os[dat$pfs_status == 0] = 0
  dat$os_status[dat$pfs_status == 0] =   dat$pfs_status[dat$pfs_status == 0]

  # if PFS is uncensored OS = PFS + SPP
  dat$os[dat$pfs_status == 1] =  dat$pfs[dat$pfs_status == 1] + d_spp[dat$pfs_status == 1]
  # uncersored PFS and  censored spp OS status  = 0
  dat$os_status[dat$pfs_status == 1 & c_spp ==0] = 0
  # uncersored PFS and SPP OS status  = 1
  dat$os_status[dat$pfs_status == 1 & c_spp ==1] = 1

  dat$treatment = treat
  dat$enrollment_time = etim

 return(list(data = dat, beta_p = beta_p, beta_s = beta_s, effect = effect, spp = d_spp, spp_status = c_spp))

}
