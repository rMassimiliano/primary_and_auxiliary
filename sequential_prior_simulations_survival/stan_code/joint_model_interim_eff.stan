functions{
  real spike_and_slab_lpdf(real b, real p)
  {
    real ldens;
      ldens = log_mix(p,normal_lpdf(b | 0, 0.001), normal_lpdf(b | 0, sqrt(0.8)));
     return(ldens);
  }
}


data{
int n_IA_spp_censored; // number of SPP at interim censored
int n_IA_spp_uncensored; // number of SPP at interim uncensored
int n_future_spp; // number of psf to be observed

int n_IA_pfs_censored;    // number of censored pfs outcome events at interim, typically > primary
int n_IA_pfs_uncensored;  // number of uncensored pfs outcome events at interim, typically > primary
int n_future_pfs;         // number of pfs outcome to be obsereved

int<lower = 0, upper =1> treat_IA_censored_spp[n_IA_spp_censored]; // treatment allocation (0 - control & 1 - treated)
int<lower = 0, upper =1> treat_IA_censored_pfs[n_IA_pfs_censored]; // treatment allocation (0 - control & 1 - treated)

int<lower = 0, upper =1> treat_IA_uncensored_spp[n_IA_spp_uncensored]; // treatment allocation (0 - control & 1 - treated)
int<lower = 0, upper =1> treat_IA_uncensored_pfs[n_IA_pfs_uncensored]; // treatment allocation (0 - control & 1 - treated)


int<lower = 0, upper =1> treat_future_pfs[n_future_pfs]; // treatment allocation (0 - control & 1 - treated)
int<lower = 0, upper =1> treat_future_spp[n_future_spp]; // treatment allocation (0 - control & 1 - treated)
//inputs darta

real<lower =0> pfs_interim_uncensored[n_IA_pfs_uncensored];
real<lower =0> pfs_interim_censored[n_IA_pfs_censored];
real<lower =0> spp_interim_uncensored[n_IA_spp_uncensored];
real<lower =0> spp_interim_censored[n_IA_spp_censored];
}

parameters{
real intercept_pfs;
real intercept_spp;
real slope_pfs;
real beta_r;
}

model
{
// likelihood PFS censored
for(ic in 1:n_IA_pfs_censored)
{
 target += exponential_lccdf(pfs_interim_censored[ic] | exp(-intercept_pfs - slope_pfs*treat_IA_censored_pfs[ic]));
}

// likelihood PFS uncensored
for(iu in 1:n_IA_pfs_uncensored)
{
 pfs_interim_uncensored[iu] ~ exponential(exp(-intercept_pfs - slope_pfs*treat_IA_uncensored_pfs[iu]));
}

// likelihood SPP censored
for(jc in 1:n_IA_spp_censored)
{
 target += exponential_lccdf(spp_interim_censored[jc] | exp(-intercept_spp -beta_r * slope_pfs*treat_IA_censored_spp[jc]));
}

// likelihood SPP uncensored
for(ju in 1:n_IA_spp_uncensored)
{
spp_interim_uncensored[ju] ~ exponential( exp(-intercept_spp -beta_r * slope_pfs*treat_IA_uncensored_spp[ju]));
}

// prior
  intercept_pfs ~ normal(6.20, sqrt(0.5));
  intercept_spp~ normal(4.87, sqrt(0.5));
  slope_pfs~ spike_and_slab(0.10);
  beta_r ~ beta(6,1);
}
generated quantities {
real<lower=0> pfs_future[n_future_pfs]; 
real<lower=0> spp_future[n_future_spp]; 

 for(i in 1:n_future_pfs)
 {
  pfs_future[i] = exponential_rng(exp( -intercept_pfs - slope_pfs*treat_future_pfs[i]));
 }

  for(j in 1:n_future_spp)
  {
   spp_future[j] = exponential_rng(exp(-intercept_spp - beta_r*slope_pfs*treat_future_spp[j]));
  }
}
