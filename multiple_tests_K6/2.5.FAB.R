
suppressMessages(library(tidyverse))
library(FABInference)
## we generate a multivariate distribution as in our surrogate example
## we then use one the variable to inform the relationship on the other 
data_path = 'data/'

scenarios_names = c(
#  "H01_omega_1",
#  "H01_omega_2",
#  "H0_omega_10",
#  "H10_omega_1",
#  "H10_omega_2",
#  "H11_omega_10",
#"H01_omega_10",
# "H0_omega_1",
#   "H0_omega_2",
#   "H10_omega_10",
# "H11_omega_1",
#  "H11_omega_2",
 "Hmiss_omega_1",
 "Hmiss_omega_2",
 "Hmiss_omega_10")

K = 6
FAB_results = list()
system.time({
for(current_scenario in scenarios_names)
{
R = 5000
dec = matrix(nrow= R, ncol = K)
for(r in 1:R)
{
 dat = read.csv(sprintf('%s%s/trial_%s_%i.csv',data_path,current_scenario,current_scenario,r))
 #tilde_X = model.matrix(~C_i*K_i, data = dat)
 
 ## using data summary
 ##v = c( mean(dat$S[dat$C_i==1]),mean(dat$S[dat$K_i==2]),   mean(dat$S[dat$C_i==1 & dat$K_i==2]))
 ## using coefficient of the corresponding model on the auxiliary outcome
 
 tryCatch({
  
 tilde_X = cbind(int = 1, 
                 k2 = as.numeric(dat$K_i==2),
                 k3 = as.numeric(dat$K_i==3),
                 k4 = as.numeric(dat$K_i==4),
                 k5 = as.numeric(dat$K_i==5),
                 k6 = as.numeric(dat$K_i==6),
                 Te1 = as.numeric(dat$K_i==1 & dat$C_i==1),
                 Te2 = as.numeric(dat$K_i==2  & dat$C_i==1),
                 Te3 = as.numeric(dat$K_i==3  & dat$C_i==1),
                 Te4 = as.numeric(dat$K_i==4  & dat$C_i==1),
                 Te5 = as.numeric(dat$K_i==5  & dat$C_i==1),
                 Te6 = as.numeric(dat$K_i==6  & dat$C_i==1))	
 	
 	
 fits = glm(dat$S~tilde_X[,-1], data = dat, family = 'binomial')
 v =fits$coef[-1]
 
 fit = glmFAB(dat$Y~1,tilde_X[,-1], ~v, family ='binomial', silent = TRUE)

 Z_1 = summary(fit)$coef["Te1",1]/ summary(fit)$coef["Te1",2]
 Z_2 = summary(fit)$coef["Te2",1]/ summary(fit)$coef["Te2",2]
 Z_3 = summary(fit)$coef["Te3",1]/ summary(fit)$coef["Te3",2]
 Z_4 = summary(fit)$coef["Te4",1]/ summary(fit)$coef["Te4",2]
 Z_5 = summary(fit)$coef["Te5",1]/ summary(fit)$coef["Te5",2]
 Z_6 = summary(fit)$coef["Te6",1]/ summary(fit)$coef["Te6",2]

 p_val = 1-pnorm(c(Z_1,Z_2,Z_3,Z_4,Z_5,Z_6)) 
 dec[r,] = as.numeric(p_val <= 0.05/K)
 }, error = function(e) e)
 #if(r%%100==0) print(r)
}
FAB_results[[current_scenario]] = dec
cat(sprintf('done with %s \n', current_scenario))
}
})
save(FAB_results, file ="FAB_results.rdata")
t(sapply(FAB_results, \(x) apply(x, 2, mean, na.rm = TRUE)))


##FAB_results_SS = list()
##system.time({
##for(current_scenario in scenarios_names)
##{
##R = 5000
##dec = matrix(nrow= R, ncol = 2)
##for(r in 1:R)
##{
## dat = read.csv(sprintf('%s%s/trial_%s_%i.csv',data_path,current_scenario,current_scenario,r))
## tilde_X = model.matrix(~C_i*K_i, data = dat)
## 
## ## using data summary
## ##v = c( mean(dat$S[dat$C_i==1]),mean(dat$S[dat$K_i==2]),   mean(dat$S[dat$C_i==1 & dat$K_i==2]))
## ## using coefficient of the corresponding model on the auxiliary outcome
## 
## 
## 
## 
## tryCatch({
##  
## tilde_X = cbind(int = 1, 
##                 k2 = as.numeric(dat$K_i==2),
##                 Te1 = as.numeric(dat$K_i==1 & dat$C_i==1),
##                 Te2 = as.numeric(dat$K_i==2  & dat$C_i==1)
##                   )	
## 	
## 	
## fits = glm(dat$S~tilde_X[,-1], data = dat, family = 'binomial')
## v =fits$coef[-1]
## 
## fit = glmFAB(dat$Y~1,tilde_X[,-1], ~v, family ='binomial', silent = TRUE)
##
## Z_1 = summary(fit)$coef["Te1",1]/ summary(fit)$coef["Te1",2]
## Z_2 = summary(fit)$coef["Te2",1]/ summary(fit)$coef["Te2",2]
##
## p_val = 1-pnorm(c(Z_1,Z_2)) 
## dec[r,] = as.numeric(p_val <= 0.05/2)
## }, error = function(e) e)
## #if(r%%100==0) print(r)
##}
##FAB_results_SS[[current_scenario]] = dec
##cat(sprintf('done with %s \n', current_scenario))
##}
##})
##save(FAB_results_SS, file ="FAB_results_SS.rdata")
##t(sapply(FAB_results_SS, \(x) apply(x, 2, mean, na.rm = TRUE)))





