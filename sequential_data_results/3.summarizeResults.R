my_summary = function(x)
{
 out_level  = c('efficacy-stop', 'futility-stop',            'H0',            'H1')
 pow =  mean(x %in% c('efficacy-stop','H1')) 
 pow_1 = mean(x %in% 'efficacy-stop') 
 pow_2 = mean(x %in% 'H1') 
 E_N = mean(x %in% c('efficacy-stop','futility-stop'))*100 + mean(x %in% c('H0','H1'))*200
 res = c(pow, pow_1, pow_2,E_N)
 return(res)
}

Scenario_names = c("H0","H01","H10","H11","Hmiss")
{
suppressMessages(library("tidyverse"))
load("A_results.rdata")
load('freq_analysis.rdata'); 
load('surrogate_analysis.rdata')

 sink(file = "table_sec6.tex")

cat("\\begin{tabular}{r|cc|cc|cc} \n")
cat("\\toprule \n")

for(c_scenario  in Scenario_names)
{
n1 = paste0(c_scenario,"_omega_1")
n2 = paste0(c_scenario,"_omega_2")
n10 = paste0(c_scenario,"_omega_10")

tmp_1 = rbind(my_summary(A_results[[n1]]),
              my_summary(freq_analysis[[n1]]),
              my_summary(surrogate_analysis[[n1]]))

tmp_2 = rbind(my_summary(A_results[[n2]]),
              my_summary(freq_analysis[[n2]]),
              my_summary(surrogate_analysis[[n2]]))


tmp_10 = rbind(my_summary(A_results[[n10]]),
              my_summary(freq_analysis[[n10]]),
              my_summary(surrogate_analysis[[n10]]))



cat(sprintf("\\multicolumn{7}{c}{Scenario %i}\\\\ \n", which(Scenario_names == c_scenario ))) 
cat("\\midrule \n")
cat("& \\multicolumn{2}{c}{$R_{1,c}= 1$} & \\multicolumn{2}{c}{$R_{1,c} = 2$} & \\multicolumn{2}{c}{$R_{1,c} = 10$} \\\\ \n") 
cat("Method &  Power &   $\\mathbb E[N]$ &
               Power &   $\\mathbb E[N]$ &
               Power &   $\\mathbb E[N]$  \\\\ \n")
cat("Auxiliary-Augmented &")
cat(sprintf("%.3f  (%.3f; %.3f) & %.1f &", tmp_1[1,1], tmp_1[1,2], tmp_1[1,3], tmp_1[1,4]))
cat(sprintf("%.3f  (%.3f; %.3f) & %.1f &", tmp_2[1,1], tmp_2[1,2], tmp_2[1,3], tmp_2[1,4]))
cat(sprintf("%.3f  (%.3f; %.3f) & %.1f \\\\ \n", tmp_10[1,1], tmp_10[1,2], tmp_10[1,3], tmp_10[1,4]))
cat("Primary-only &")
cat(sprintf("%.3f  (%.3f; %.3f) & %.1f &", tmp_1[2,1], tmp_1[2,2], tmp_1[2,3], tmp_1[2,4]))
cat(sprintf("%.3f  (%.3f; %.3f) & %.1f &", tmp_2[2,1], tmp_2[2,2], tmp_2[2,3], tmp_2[2,4]))
cat(sprintf("%.3f  (%.3f; %.3f) & %.1f \\\\ \n", tmp_10[2,1], tmp_10[2,2], tmp_10[2,3], tmp_10[2,4]))

cat("Auxiliary-only &")
cat(sprintf("%.3f  (%.3f; %.3f) & %.1f &", tmp_1[3,1], tmp_1[3,2], tmp_1[3,3], tmp_1[3,4]))
cat(sprintf("%.3f  (%.3f; %.3f) & %.1f &", tmp_2[3,1], tmp_2[3,2], tmp_2[3,3], tmp_2[3,4]))
cat(sprintf("%.3f  (%.3f; %.3f) & %.1f \\\\ \n", tmp_10[3,1], tmp_10[3,2], tmp_10[3,3], tmp_10[3,4]))


cat("\\bottomrule \n")
}
cat("\\end{tabular} \n")
}





