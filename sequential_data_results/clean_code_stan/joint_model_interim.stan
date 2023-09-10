functions{
  real spike_and_slab_lpdf(real b, real p)
  {
    real ldens;
      ldens = log_mix(p,normal_lpdf(b | 0, 0.001), normal_lpdf(b | 0, sqrt(0.8)));
     return(ldens);
  }
}


data{
int n; //total sample size
int ny_interim; // number of primary outcome events at interim
int ny_future; // number of primary outcome to be observed
int ns_interim; // number of auxiliary outcome events at interim, typically > primary
int ns_future; // number of auxiliary outcome to be obsereved


int<lower=0, upper=1> y_interim[ny_interim]; 
int<lower=0, upper=1> s_interim[ns_interim]; 


int<lower = 0, upper =1> c[n];

//real prob_ss0;

}

parameters{
real intercept_Y;
real intercept_S;
real<lower = 0, upper =1> beta;
real slope_S;
vector[n] eps;
}

model
{
 for( i in 1:ny_interim)
 {
  y_interim[i] ~ bernoulli(inv_logit(intercept_Y + beta*slope_S * c[i]  + eps[i]));
  }
  for(j in 1:ns_interim)
  {
   s_interim[j] ~ bernoulli(inv_logit(intercept_S + slope_S * c[j] + eps[j]));
  }

 intercept_Y ~ normal(-1.5, sqrt(0.5));
 intercept_S ~ normal(-0.8, sqrt(0.5));
 //slope_S     ~ spike_and_slab(prob_ss0);
 slope_S     ~ spike_and_slab(0.10);

 beta ~ beta(6,1);
 for(i in 1:n)
 {
  eps[i] ~ normal(0,1);
 }
 
}

generated quantities {

int<lower=0, upper=1> y_future[ny_future]; 
int<lower=0, upper=1> s_future[ns_future]; 


 for( i in 1:ny_future)
 {
  y_future[i] = bernoulli_rng(inv_logit(intercept_Y + beta*slope_S * c[ny_interim + i]  + eps[ny_interim + i]));
  }
  for(j in 1:ns_future)
  {
   s_future[j] = bernoulli_rng(inv_logit(intercept_S + slope_S * c[ns_interim + j] + eps[ns_interim + j]));
  }
}
