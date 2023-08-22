sample_posterior_predictive = function(posterior_parameters,prior, N = 100)
{
 param = prior 

 #sample Y, logit(b), S_intercept, S_slab 
posterior_samples = mvtnorm::rmvnorm(1,posterior_parameters$mode, posterior_parameters$var)
posterior_samples[,2] = plogis(posterior_samples[2])
param$Y_intercept[1] = posterior_samples[1]
param$Y_effect_reduction = posterior_samples[2]
param$S_intercept[1] = posterior_samples[3]
param$S_effect_slab[1] = posterior_samples[4]
param$N = N

return(prior_data_generation(param)$data)

}
	
