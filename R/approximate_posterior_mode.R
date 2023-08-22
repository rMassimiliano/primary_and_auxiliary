#' function to approximate the posterior mode and variance of our joint model for S and Y
#'@param `data`: Clinical trial data: a data frame have Y,S and C_i
#'@param `prior`: A list of the prior hyperparameters; require:
# effect_prob, in the paper is \xi, ex omega  in the code
#  re_var: variance of the latent variable Z ex, simga2_z,
# S_intercept[2]: mean and variance of the intercept of S
# S_effect_slab[2] =  mean and variance of the slope of S, if non zero
# Y_intercept[2] =   mean and var for the intercept of Y
# Y_slope_var =    var for the slope of Y
#ln_prior1[2], ln_prior1[2], eps
#'

#'@param `QuadPoint`: number of points used for the quadrature, default 100
approximate_posterior = function(data, prior, nQuadPoint = 100)
{
## we assume b param[2]
	require(numDeriv)
	require(fastGHQuad)
	rule100 <- gaussHermiteData(nQuadPoint)

 log_posterior_density = function(param,data, prior) 
 {
   N = NROW(data)
   ## likelihood contribution
   l_i = function(x,param,data,prior,i)
   {
    dbinom(data$S[i], 1, prob = plogis(param[3] + param[4]*I(data$C_i[i] ==1) + x)) *
  dbinom(data$Y[i], 1, prob = plogis(param[1] + plogis(param[2])*param[4]*I(data$C_i[i] ==1) +x)) * dnorm(x,0,sqrt(prior$re_var))
   }
   ## laplace quadrature to compute the likelihood
    log_likelihood = sum(log(sapply(1:N, \(i) aghQuad(l_i, 0, 1, rule100, param = param, data = data, prior = prior, i =i))))
    ## log_posterior = sum(log(sapply(1:N, \(i) integrate(l_i, -Inf, Inf,  param = param, data = data, prior = prior, i =i)$value)))
   ## evaluate the prior
    log_prior = dnorm(param[3],prior$S_intercept[1], sqrt(prior$S_intercept[2]) ,log = TRUE) +
   	(1-prior$effect_prob) * dnorm(param[4], prior$S_effect_slab[1], sqrt(prior$S_effect_slab[2]) ,log = TRUE) +
   	dnorm(param[1],prior$Y_intercept[1], sqrt(prior$Y_intercept[2]) ,log = TRUE)+ dbeta(plogis(param[2]),prior$b_a, prior$b_b ,log = TRUE)
   
    log_posterior = log_likelihood + log_prior
    return(log_posterior)
    }



### let's optimize the posterior and get the mode and the inverse of the second derivative
## We use prior mean as a starting value
starting_values = with(prior, c(Y_intercept[1],
                                qlogis(b_a/(b_a + b_b)),
                                S_intercept[1],
                                (1-effect_prob)* S_effect_slab[1]))
post_mode =  optim(starting_values, function(x) -log_posterior_density(x,data, prior))$par
names(post_mode) = c("Y_intercept","logit(b)","S_intercept","S_slope")
posterior_hessian = hessian(function(x) -log_posterior_density(x,data, prior), post_mode)
post_variance = solve(posterior_hessian)
return(list(mode = post_mode, var = post_variance))
}

