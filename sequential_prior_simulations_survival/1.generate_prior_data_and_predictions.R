library(dplyr)
library(rstan)
library(survival)
library(coin)
library(parallel)
model = readRDS("stan_code/joint_model_interim_eff.rds")
source("../../R/generate_data_from_prior_survival.R")

do_one_sim = function(r)
{
 set.seed(123 + r)
 param = list(N = 200,
	     S_intercept = c(6.20,0.5),
             S_effect_slab = c(0,0.8),
             effect_prob = 0.1,
	     SPP_log_mean = c(4.87,0.5),
	     m_patients = 4,
	     p_pfs = 0.2,
             p_spp = 0.2)

 priorData = prior_data_generation_surival(param)

 priorData$data$pid = 1:NROW(priorData$dat)
 IA_data = priorData$data
 dat = priorData$dat


IA_data = IA_data |> mutate(os_calendar_time = os + enrollment_time)

#time of the intermi analysis
event_th = min(50,sum(IA_data$os_status))
t_ia =  IA_data |>  arrange(os_calendar_time) |> mutate(cum_events = cumsum(os_status)) |> filter(cum_events == event_th) |> pull(os_calendar_time) |> last()

t_ia =  min(t_ia, max(IA_data$enrollment_time))


## censor pfs events
IA_data = IA_data |> filter(enrollment_time <= t_ia)
IA_data = IA_data |> mutate(pfs_calendar_time = pfs + enrollment_time)
IA_data$pfs_status[IA_data$pfs_calendar_time> t_ia] = 0
IA_data$pfs[IA_data$pfs_calendar_time> t_ia] = t_ia - IA_data$enrollment_time[IA_data$pfs_calendar_time> t_ia]


## censor events that happened after t_ia
IA_data$os_status[which(IA_data$os_calendar_time> t_ia)] = 0
IA_data$os[which(IA_data$os_calendar_time> t_ia)] = t_ia - IA_data$enrollment_time[which(IA_data$os_calendar_time> t_ia)]

## pfs censored are also os censored at the same time
IA_data$os[IA_data$pfs_status ==0] = IA_data$pfs[IA_data$pfs_status ==0] 
IA_data$os_status[IA_data$pfs_status ==0] = 0

## fix surival post progression
IA_data$spp  = IA_data$os - IA_data$pfs
IA_data$spp_status = IA_data$os_status * IA_data$pfs_status
IA_data$spp[(IA_data$os_status == 0) &  (IA_data$pfs_status ==0)] = NA
IA_data$spp_status[(IA_data$os_status == 0) &  (IA_data$pfs_status ==0)] = NA



## add subecjet that need to be predicted
spp_prediction_inds = IA_data$pid[which(IA_data$spp_status == 0)]
pfs_prediction_inds = IA_data$pid[which(IA_data$pfs_status == 0)]
tmp = dat[!(dat$pid %in% IA_data$pid),]$pid
spp_prediction_inds = c(spp_prediction_inds, tmp)
pfs_prediction_inds = c(pfs_prediction_inds, tmp)

  stan_input = list(
        n_IA_spp_censored =  sum(IA_data$spp_status==0, na.rm = TRUE),
        n_IA_spp_uncensored =  sum(IA_data$spp_status, na.rm = TRUE),
        n_future_spp = length(spp_prediction_inds),
        n_future_pfs = length(pfs_prediction_inds),
        n_IA_pfs_censored = sum(IA_data$pfs_status==0),
        n_IA_pfs_uncensored = sum(IA_data$pfs_status),
        treat_IA_censored_pfs = IA_data$treat[IA_data$pfs_status ==0],
        treat_IA_uncensored_pfs = IA_data$treat[IA_data$pfs_status ==1],
        treat_IA_censored_spp = IA_data$treat[which(IA_data$spp_status ==0)],
        treat_IA_uncensored_spp = IA_data$treat[which(IA_data$spp_status ==1)],
        treat_future_pfs = dat$treatment[dat$pid %in% pfs_prediction_inds],
        treat_future_spp = dat$treatment[dat$pid %in% spp_prediction_inds],
        pfs_interim_uncensored = IA_data$pfs[IA_data$pfs_status ==1],
        pfs_interim_censored = IA_data$pfs[IA_data$pfs_status ==0],
        spp_interim_uncensored = IA_data$spp[which(IA_data$spp_status ==1)],
        spp_interim_censored = IA_data$spp[which(IA_data$spp_status ==0)])
  


  suppressWarnings({suppressMessages({fit = sampling(model, stan_input, iter = 1000, chains = 3, verbose = FALSE,show_messages = FALSE,refresh = 0)})})
  params = extract(fit)
  



priorData$t_ia = t_ia
priorData$IA_data = IA_data
priorData$IA_posterior = params


ch_ind = 0:1499 %/% 500 + 1
ch_id = which.max(tapply(params$lp__, ch_ind, mean))

   PFS_f = params$pfs_future[ch_ind == ch_id,] 
   SPP_f = params$spp_future[ch_ind == ch_id,]  
   priorData$Z = numeric(NROW(PFS_f))
   priorData$pfs_prediction_inds = pfs_prediction_inds
   priorData$spp_prediction_inds = spp_prediction_inds
  for(b in seq(NROW(PFS_f)))
  {
  #choose a chain 
	  cdata = dat
	  cdata$spp = NA
	  ## sanity check

	  cdata[cdata$pid %in% pfs_prediction_inds,]$pfs = PFS_f[b,]
	  cdata[cdata$pid %in% spp_prediction_inds,]$spp = SPP_f[b,]
	  cdata$os[!is.na(cdata$spp)] = cdata$pfs[!is.na(cdata$spp)] + cdata$spp[!is.na(cdata$spp)]
cdata$os_status[!is.na(cdata$spp)] = 1
## we use positive sign for the test
priorData$Z[b] =  logrank_test(Surv(os, os_status) ~ factor(treatment==0),data = cdata)@statistic@teststatistic
    }

saveRDS(priorData, file = sprintf("data/prior_%i.rds",r))
cat(sprintf("done %i/%i\r",r,1000))
}

cl = makeCluster(10,outfile="")
clusterEvalQ(cl,{
library(dplyr)
library(rstan)
library(survival)
library(coin)
model = readRDS("stan_code/joint_model_interim_eff.rds")
source("../../R/generate_data_from_prior_survival.R")
})
clusterExport(cl, "do_one_sim")

R = 5000
parSapply(cl,1:R,  do_one_sim)
stopCluster(cl)


