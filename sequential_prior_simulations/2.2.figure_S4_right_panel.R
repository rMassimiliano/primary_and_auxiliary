library(dplyr)
library(tidyr)
library(ggplot2)
library(gsDesign)
library(fields)


source("../R/generate_data_from_prior.R")
source("../R/approximate_posterior_mode.R")
source("../R/sample_posterior_predictive.R")
source("../R/compute_Zs.R")

## extract
Te_to_Zy = \(x) {res = x["Delta_Y"]/sqrt(x["Var_Delta_Y"]); names(res) =NULL;res }



priorValues = lst( 
     S_intercept = c(-0.8,0.5),
     effect_prob = 0.10,
     S_effect_slab = c(0,0.8),
     Y_intercept = c(-1.5,0.5),
     Y_effect_reduction = 0.8,
     re_var      = 1,
     N = 200,
     K = 1,
     p_k = 1,
     Y_a = 6,
     Y_b = 1,
     b_a = 6,
     b_b = 1)

#### compute grid of boundaries for efficacy stopping
#gam_seq = c(-4,-2,-1,0,1,2,3,4)
gam_seq  = seq(-5,5, l = 30)
bounds_sf = tibble()
for(gam in gam_seq)
{ 
	mod = gsDesign(k = 2,
		      test.type = 1, 
		      n.fix = 200,
		      alpha = 0.05,
		      beta =0.2,
		      sfupar = gam,
		      endpoint = 'Binomial')
bounds_sf =  bounds_sf |>
             bind_rows(
		       tibble(gamma  = gam,
			      c1     = mod$upper$bound[1],
			      c2     = mod$upper$bound[2],
			      alpha1 = mod$upper$spend[1],
			      alpha2 = mod$upper$spend[2])
	               )
}


N_IA = priorValues$N/2

prior_decision = tibble(effect = numeric(), IA_efficacy= numeric(), FA_efficacy= numeric(), IA_PPR= numeric())
for(r in 1:1000){
load(sprintf("results/prior_simulation_%i.rdata",r))
effect = results$priorSim$effect
## Z interim and final analyis
Z_Y_IA =  Te_to_Zy(compute_Zs(results$priorSim$data[1:N_IA,]))
Z_Y_FA =  Te_to_Zy(compute_Zs(results$priorSim$data))

for(j in 1:NROW(bounds_sf))
{
  pred_p = mean(apply(results$PP_sim,1,\(x) x[1]/sqrt(x[3])) >= bounds_sf$c2[j])
  IA_dec = as.numeric(Z_Y_IA >= bounds_sf$c1[j])
  FA_dec = as.numeric(Z_Y_FA >= bounds_sf$c2[j])
  prior_decision = prior_decision |>
                  bind_rows(tibble(gamma = bounds_sf$gamma[j],
				   effect = effect,
				   IA_efficacy= IA_dec,
				   FA_efficacy= IA_dec,
				   IA_PPR= pred_p))
}
if(r%%50==0) print(r)
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


th_seq = seq(0,1, l = 10)
th_seq = sort(c(th_seq,0.95,0.97,0.99))
prior_decision = prior_decision |> mutate(ExpU = 0) 

Utility = tibble()

for(current_t in th_seq)
{
 for(r in 1:NROW(prior_decision))
 {
    dec = get_decision(current_t, prior_decision$IA_PPR[r], prior_decision$IA_efficacy[r],prior_decision$FA_efficacy[r])
    Utility = Utility |> bind_rows(tibble(iter = r, gamma = prior_decision$gamma[r], beta = current_t, ExpU = compute_utility(prior_decision$effect[r],dec,N_IA=100,N_FA = 200,lambda = 0.00005)))
 }
 cat(sprintf("Done %i out of %i \n",which(current_t == th_seq),length(th_seq)))
}



plot_data = Utility |>
            group_by(gamma,beta) |>
            summarize(ExpU = mean(ExpU)) 

	    m  = as.matrix(pivot_wider(plot_data,names_from = gamma, values_from = ExpU)[,-1])

my_obj = list(x = th_seq , y = gam_seq, z = m)
my_loc = list(x =seq(0,1,length=101), y=seq(-5,5,length=101))

interpolated_data = interp.surface.grid(my_obj, my_loc )

dimnames(interpolated_data$z) <- list(x=interpolated_data$x,y=interpolated_data$y)


reshape2::melt(interpolated_data$z, value.name = 'z') |> 
ggplot(aes(x,y,z=z,fill=z)) +
geom_raster(interpolate = TRUE) + 
scale_fill_gradient2()+
theme_classic(11) +
geom_hline(yintercept = 2, lty = 'dashed') +
geom_vline(xintercept = 0.13, lty = 'dashed') + 
labs(x = 'beta (futility)', y = 'beta (efficacy)', fill = 'Utility')+
scale_x_continuous(expand = c(0.001,0.001)) +
scale_y_continuous(expand = c(0.001,0.001)) +
theme(legend.position = 'top',legend.key.width = unit(1.6, 'cm')) 
ggsave("optimal_solution_seq_biv.png", width = 7, height =  5.1 )







