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

int<lower=0, upper=1> y_interim[ny_interim]; 



int<lower = 0, upper =1> c[n];

}

parameters{
real intercept_Y;
real slope_Y;
}

model
{
 for( i in 1:ny_interim)
 {
  y_interim[i] ~ bernoulli(inv_logit(intercept_Y + slope_Y * c[i]));
  }

 intercept_Y ~ normal(-1.5, sqrt(0.5));
 slope_Y   ~ spike_and_slab(0.10);
}

generated quantities 
{
 int<lower=0, upper=1> y_future[ny_future]; 
 for( i in 1:ny_future)
 {
  y_future[i] = bernoulli_rng(inv_logit(intercept_Y + slope_Y * c[ny_interim + i]));
 }
}
