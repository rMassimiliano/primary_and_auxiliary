functions{
  real spike_and_slab_lpdf(real b, real p)
  {
    real ldens;
      ldens = log_mix(p,normal_lpdf(b | 0, 0.001), normal_lpdf(b | 0, sqrt(0.8)));
     return(ldens);
  }


  real hypoexponential_lpdf(real y, real rate1,real rate2) {

  real res;
  res  = (exp(-y*rate2) - exp(-y*rate1)) *(rate1*rate2)/(rate1 - rate2);
  return log(res);
  }
 
 real hypoexponential_rng(real rate1, real rate2) {
    real y = 0;
     y = exponential_rng(rate1) + exponential_rng(rate2);
    return y;
  }


  real hypoexponential_lccdf(real y, real rate1,real rate2) {
      real res;
      res = rate2/(rate2 -rate1)*exp(-y*rate1)  - rate1/(rate2 -rate1)*exp(-y*rate2);
      return log(res);
  }

}


data{
int n_IA_censored; // number of SPP at interim censored
int n_IA_uncensored; // number of SPP at interim uncensored
int n_future; // number of psf to be observed

int<lower = 0, upper =1> treat_IA_censored[n_IA_censored]; // treatment allocation (0 - control & 1 - treated)
int<lower = 0, upper =1> treat_IA_uncensored[n_IA_uncensored]; // treatment allocation (0 - control & 1 - treated)



int<lower = 0, upper =1> treat_future[n_future]; // treatment allocation (0 - control & 1 - treated)
//inputs darta
real<lower =0> outcome_interim_uncensored[n_IA_uncensored];
real<lower =0> outcome_interim_censored[n_IA_censored];
}

parameters{
real intercept;
real intercept2;
real slope;
}

model
{
// likelihood outcome censored
for(ic in 1:n_IA_censored)
{
 //target += exponential_lccdf(outcome_interim_censored[ic] | exp(-intercept - slope*treat_IA_censored[ic]));
   target += hypoexponential_lccdf(outcome_interim_censored[ic] | exp(-intercept - slope*treat_IA_censored[ic]), exp(-intercept2));

}

// likelihood outcome uncensored
for(iu in 1:n_IA_uncensored)
{
 target += hypoexponential_lpdf(outcome_interim_uncensored[iu]|exp(-intercept - slope*treat_IA_uncensored[iu]),exp(-intercept2));
 //outcome_interim_uncensored[iu] ~ exponential(exp(-intercept - slope*treat_IA_uncensored[iu]));
}


// prior
  intercept ~ normal(6.20, sqrt(0.5));
  intercept2 ~ normal(4.87, sqrt(0.5));
  slope~ spike_and_slab(0.10);

}
generated quantities {
real<lower=0> outcome_future[n_future]; 

 for(i in 1:n_future)
 {
  //outcome_future[i] = exponential_rng(exp( -intercept - slope*treat_future[i]));
outcome_future[i] = hypoexponential_rng(exp( -intercept - slope*treat_future[i]),exp(-intercept2));
 }

}


