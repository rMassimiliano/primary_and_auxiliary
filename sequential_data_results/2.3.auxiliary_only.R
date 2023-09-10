suppressMessages({
 library(optparse);
 library(rstan);rstan_options(auto_write = TRUE);
 library(gsDesign)
  })


## code to use scenario name as input
#option_list = list(
#    make_option(c("-s","--scenario"), type="character", default=NULL, 
#              help="name of the simulation scenario", metavar="character")
#) 
#opt_parser = OptionParser(option_list=option_list)
#opt = parse_args(opt_parser)
#current_scenario = opt$scenario



scenario_list = c('H01_omega_1',    'H0_omega_1'     'H10_omega_1'    'H11_omega_1', 'Hmiss_omega_1', 'H01_omega_10', 'H0_omega_10', 'H10_omega_10', 'H11_omega_10', 'Hmiss_omega_10', 'H01_omega_2', 'H0_omega_2', 'H10_omega_2', 'H11_omega_2', 'Hmiss_omega_2')

  do_one_sim = function(r,...) 
  {
   dat = read.csv(sprintf('%s%s/trial_%s_%i.csv',data_path,current_scenario,current_scenario,r))
   ## compute the Zy for interim and final
  
   Zy_IA = with(dat[1:100,], (mean(S[C_i ==1]) - mean(S[C_i ==0]))/sqrt(var(S[C_i ==1])/sum(C_i==1) + var(S[C_i ==0])/sum(C_i==0)))
   Zy_FA = with(dat, (mean(S[C_i ==1]) - mean(S[C_i ==0]))/sqrt(var(S[C_i ==1])/sum(C_i==1) + var(S[C_i ==0])/sum(C_i==0)))
  
 stan_input = list(y_interim = dat$S[1:100],
                     c = dat$C_i,
                     n =NROW(dat),
                     ny_interim =100,
                     ny_future = 100)
                   
  suppressMessages({fit = sampling(model, stan_input, iter = 1000, chains = 1, verbose = FALSE,show_messages = FALSE,refresh = 0)})
  params = extract(fit)

  Z_p = numeric(500)
 for(b in 1:500)
 { 
  Y_p = c(dat$Y[1:100],params$y_future[b,])
  C_p = dat$C_i 
  Z_p[b] = (mean(Y_p[C_p ==1]) - mean(Y_p[C_p ==0]))/sqrt(var(Y_p[C_p ==1])/sum(C_p==1) + var(Y_p[C_p ==0])/sum(C_p==0))
 }
      if(mean(Z_p >= C2)<= BETA)  dec  = 'futility-stop'
       else if(Zy_IA>=C1) dec  = 'efficacy-stop'
       else if(Zy_FA>= C2) dec = 'H1'
       else dec ='H0'

   return(dec)
}


data_path = 'data/'
model = readRDS("clean_code_stan/marginal_model_interim.rds")




### optimal solution + parameters
BETA = 0.13
GAMMA = 2
N_IA = 100
mod = gsDesign(k = 2, test.type = 1, n.fix = 200, alpha = 0.05, beta =0.2, sfupar = GAMMA, endpoint = 'Binomial')
C1     = mod$upper$bound[1]
C2     = mod$upper$bound[2]



for(current_scenario in scenario_list)
{
   R = 5000
   decision = numeric(R)
   for(r in 1:R)
   {
   decision[r] =  do_one_sim(r)
   if(r%%500 == 0) cat(sprintf("done with simulation %i\n",r))
   }
   saveRDS(decision, file =  paste0("results/auxiliary_only/","auxiliary_only_",current_scenario,".rds") )
   cat(sprintf('done with %s \n', current_scenario))
}



