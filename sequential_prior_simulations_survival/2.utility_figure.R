library(dplyr)
library(tidyr)
library(ggplot2)
library(gsDesign)
library(fields)
#library(rstan)
library(survival)
library(coin)
#model = readRDS("stan_code/joint_model_interim.rds")
#source("../R/generate_data_from_prior_survival.R")


param = list(N = 200,
	     S_intercept = c(6.20,0.5),
             S_effect_slab = c(0,0.8),
             effect_prob = 0.1,
	     SPP_log_mean = c(4.87,0.5),
	     m_patients = 4,
	     p_pfs = 0.2,
             p_spp = 0.2)

#### compute grid of boundaries for efficacy stopping
gam_seq = seq(-5,5, l = 30) 

bounds_sf = tibble()
for(gam in gam_seq)
{ 
	mod = gsDesign(k = 2,
		      test.type = 1, 
		      n.fix = 200,
		      alpha = 0.05,
		      beta =0.2,
		      sfupar = gam,
		      endpoint = 'TTE')
bounds_sf =  bounds_sf |>
             bind_rows(
		       tibble(gamma  = gam,
			      c1     = mod$upper$bound[1],
			      c2     = mod$upper$bound[2],
			      alpha1 = mod$upper$spend[1],
			      alpha2 = mod$upper$spend[2])
	               )
}


prior_decision = tibble(effect = numeric(), IA_efficacy= numeric(), FA_efficacy= numeric(), IA_PPR= numeric(), N_IA = numeric())

for(r in 1:5000){
priorData = 	readRDS(sprintf("data/prior_%i.rds",r))
effect = priorData$effect
## Z interim and final analyis
## model for interim and final analysis


Z_Y_IA = logrank_test(Surv(os, os_status) ~ factor(treatment==0), data = priorData$IA_data)@statistic@teststatistic
Z_Y_FA =  logrank_test(Surv(os, os_status) ~ factor(treatment==0), data = priorData$data)@statistic@teststatistic




for(j in 1:NROW(bounds_sf))
{
  pred_p = mean(priorData$Z>= bounds_sf$c2[j])
  IA_dec = as.numeric(Z_Y_IA >= bounds_sf$c1[j])
  FA_dec = as.numeric(Z_Y_FA >= bounds_sf$c2[j])
  prior_decision = prior_decision |>
                  bind_rows(tibble(gamma = bounds_sf$gamma[j],
				   effect = effect,
				   IA_efficacy= IA_dec,
				   FA_efficacy= IA_dec,
				   IA_PPR= pred_p,
				   N_IA = NROW(priorData$IA_data)))
}
if(r%%50==0) cat(sprintf("%i/5000\r",r))
}


prior_decision |> group_by(gamma) |> summarize_all(mean)


get_decision = function(th,IA_PPR,IA_eff,FA_eff)
{
  if(IA_PPR <= th) 
  {
   return("futility-stop")
  }
  else if(IA_eff == 1)
  {
   return("efficacy-stop")
  }
  else if(FA_eff==1)
  {
   return("H1")
  }
  else return("H0")
}


## lets compute the utility
compute_utility = function(effect,decision, N_IA=100,N_FA = 200,lambda = 0.00005)
{
  if(effect == 0)
  {
    if(decision == 'futility-stop' | decision == 'efficacy-stop' )  return(-lambda*N_IA)
    else if(decision == 'H0' | decision == 'H1' )  return(-lambda*N_FA)
  } else
  {
    if(decision == 'futility-stop') return(-lambda*N_IA)
    else if(decision == 'efficacy-stop') return(1-lambda*N_IA)
    else if(decision == 'H1') return(0.5-lambda*N_FA)
    else if(decision == 'H0') return(-lambda*N_FA)
  }
}


th_seq = seq(0,1, l = 20)
#th_seq = sort(c(th_seq,0.95,0.97,0.99))
prior_decision = prior_decision |> mutate(ExpU = 0) 

Utility = tibble()

for(current_t in th_seq)
{
 for(r in 1:NROW(prior_decision))
 {
    dec = get_decision(current_t, prior_decision$IA_PPR[r], prior_decision$IA_efficacy[r],prior_decision$FA_efficacy[r])

    Utility = Utility |> bind_rows(tibble(iter = r, gamma = prior_decision$gamma[r], beta = current_t, ExpU = compute_utility(prior_decision$effect[r],dec,N_IA=prior_decision$N_IA[r], N_FA = 200,lambda = 0.00005)))
 }
 cat(sprintf("Done %i out of %i \n",which(current_t == th_seq),length(th_seq)))
}


#saveRDS(Utility, file = "utility_right_panel.rds")

Utility = readRDS("utility.rds")
plot_data = Utility |>
            group_by(gamma,beta) |>
            summarize(ExpU = mean(ExpU)) 

	    m  = as.matrix(pivot_wider(plot_data,names_from = gamma, values_from = ExpU)[,-1])

my_obj = list(x = th_seq , y = gam_seq, z = m)
my_loc = list(x =seq(min(th_seq),max(th_seq),length=101), y=seq(min(gam_seq),max(gam_seq),length=101))

interpolated_data = interp.surface.grid(my_obj, my_loc )

dimnames(interpolated_data$z) <- list(x=interpolated_data$x,y=interpolated_data$y)


reshape2::melt(interpolated_data$z, value.name = 'z') |> 
ggplot(aes(x,y,z=z,fill=z)) +
geom_raster(interpolate = TRUE) + 
scale_fill_gradientn(colours = hcl.colors(5), values = c(0,0.5,0.95,0.995,1))+
theme_classic(11) +
geom_hline(yintercept = 4.3, lty = 'dashed') +
geom_vline(xintercept = 0.1, lty = 'dashed') + 
labs(x = 'beta (futility)', y = 'beta (efficacy)', fill = 'Utility')+
scale_x_continuous(expand = c(0.001,0.001)) +
scale_y_continuous(expand = c(0.001,0.001)) +
theme(legend.position = 'top',legend.key.width = unit(1.6, 'cm')) 
ggsave("optimal_solution_seq_biv_surv.png", width = 7, height =  5.1 )









