suppressMessages(library("tidyverse"))
NDATA = 5000
bernoulli_copula = function(py,ps, omega)
{ 
    pijk = matrix(0,2,2)
    if(omega == 1)
    {
     ## independent
     pijk[2,2] = py*ps
     pijk[1,1] = (1-py)*(1-ps)
     pijk[2,1] = py*(1-ps)
     pijk[1,2] = (1-py)*ps
    }
    else
    {
      pijk = matrix(0,2,2)
      pijk[2,2] =  (1+ (omega-1)*(ps + py) - sqrt( (1 + (omega-1)*(py+ps))^2 - 4*omega * (omega-1)* py*ps))/(2*(omega-1))
      pijk[2,1] = py - pijk[2,2]
      pijk[1,2] = ps - pijk[2,2]
      pijk[1,1] = (1-py) - pijk[1,2]
    }
    
    return(pijk)
}

########################






## we consider binary correlated outcome 
## We consider a Clayton copula model for the joint distribution. Having a parameter that control the correlation


## define the marginal distribution
true_parameters = lst()
true_parameters$N =200

## number of groups
true_parameters$K = 1
## biomarker probability vector
true_parameters$p_k = 1         ## biomarker proportion

## marginals for the primary
true_parameters$pi_ykc = array(0, dim = c(true_parameters$K, 2))
## SOC
true_parameters$pi_ykc[,1] = 0.20
## no TE
##true_parameters$pi_ykc[,2] = c(0.20,0.20) 
true_parameters$pi_ykc[,2] = 0.40


## marginals for the auxiliary
true_parameters$pi_skc = array(0, dim = c(true_parameters$K, 2))

## auxiliary SOC
true_parameters$pi_skc[,1] = 0.50
## auxiliary TE
 # no effect on group 4
 # 
true_parameters$pi_skc[,2] = 0.50


## odds ration copula parameters
## 1: independent
true_parameters$rho = 2



## joint distribution of the outcomes

true_parameters$pi_ys_kc =  array(0, dim = c(2,2,true_parameters$K, 2))
    
for(k in 1:true_parameters$K)
{
 for(c in 1:2)
 { 
  true_parameters$pi_ys_kc[,,k,c] = bernoulli_copula(true_parameters$pi_ykc[k,c],true_parameters$pi_skc[k,c], omega = true_parameters$rho)
 }
}
pp = true_parameters$pi_ys_kc[,,k,c]
cov2cor(diag(diag(pp),2)  - pp %*% t(pp))

## generate data
possible_outcomes = list(c(0,0),c(1,0), c(0,1), c(1,1))

for(r in 1:NDATA)
{
trialData = data.frame(Y = numeric(true_parameters$N), S = numeric(true_parameters$N), K_i = numeric(true_parameters$N), C_i = numeric(true_parameters$N))



 for(i in 1:true_parameters$N)
 {
   ## treatment effect 0,1
   Ki      = sample(1:true_parameters$K, size = 1, prob = true_parameters$p_k)
   Ci      = rbinom(1,1,0.5) 
   outcomes =  unlist(sample(possible_outcomes, size = 1, prob = c(true_parameters$pi_ys_kc[,,Ki,Ci + 1])))
   trialData[i,] = cbind(Y = outcomes[1], S =  outcomes[2], K_i = Ki, C_i = Ci)
}	

readr::write_csv(trialData, sprintf("data/H10_omega_2/trial_H10_omega_%i_%i.csv",true_parameters$rho, r))
if(r%%100 ==0) cat(sprintf("Dataset %i \n",r))
}
