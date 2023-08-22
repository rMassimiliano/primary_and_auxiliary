#' function to perform the empirical Bayes bootstrap

#'@param `trialData`: a data.frame or tibble containing the trial data. Columns name  'Y' 'S' 'K' 'C' 
#'@param `beta`: parameter for the weights
#'@param `alpha`: The targeted bound for the FWER
#'@param `B`: number of bootstrap replicate used to calibreate the procedure
empirical_bayes_bootstrap = function(trialData,beta,alpha =0.05, B =500, method = c('cspline'))
{
 ## compute relevant data quantities
suppressMessages({stats = trialData |> group_by(K,C) |> summarize(mean_Y = mean(Y), mean_S = mean(S), var_Y = var(Y)/n(), var_S = var(S)/n(), cov_YS = cov(Y,S)/n())})

mean_S = stats$mean_S[stats$C == 1] - stats$mean_S[stats$C == 0]
var_S  = stats$var_S[stats$C == 1] + stats$var_S[stats$C == 0]
var_Y  = stats$var_Y[stats$C == 1] + stats$var_Y[stats$C == 0]
cov_YS = stats$cov_YS[stats$C == 1] + stats$cov_YS[stats$C == 0]


## empirical Bayes Estimate of the mean of the auxiliary
    eb_mean_S = ebnm(mean_S, sqrt(var_S), prior_family = 'normal', mode = "estimate")

## bootstrap
K=length(mean_S)
vcov_YS = lapply(1:K, \(k) cbind( c(var_Y[k],cov_YS[k]),c(cov_YS[k], var_S[k])))
means = cbind(0,eb_mean_S$posterior[,"mean"])
alpha_seq = c(seq(0,0.2, l =40), seq(0.2,0.5, l = 10))
boot_decisions = array(NA,dim = c(B, length(alpha_seq)))

for(b in 1:B)
{
  boot_TEs = t(sapply(1:K, \(k) rmvnorm(1,  means[k,],vcov_YS[[k]])))
  boot_p = 1-sapply(1:K,\(k) pnorm(boot_TEs[k,1]/sqrt(vcov_YS[[k]][1,1])))
  boot_weights = distribute_weights(beta,boot_TEs[,2]) 
   for(k in 1:length(alpha_seq))
   {
    boot_decisions[b,k] =  sum(boot_p<=boot_weights *alpha_seq[k])
   }
}
 FWERs = apply(boot_decisions,2, \(x) mean(x>=1))
if(method == 'cspline')
{
 alpha_seq2 = sort(unique(c(alpha_seq, seq(0,1, l = 100))))
 spi0 = smooth.spline(alpha_seq, FWERs, df = 3)
 FWER_smooth = predict(spi0, x = alpha_seq2)
 boot_alpha = alpha_seq2[which.max(FWER_smooth$y[FWER_smooth$y <= alpha])]
}
else if(method =='interpolation'){
suppressWarnings({f = spline(x = alpha_seq, y = FWERs, method = 'natural')})
boot_alpha = f$x[which.max(f$y[f$y <=alpha])]
}
else{
 boot_alpha = alpha_seq[which.max(FWERs[FWERs <= alpha])]
}
 return(boot_alpha)
}
