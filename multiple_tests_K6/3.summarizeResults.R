my_summary = function(x)
{
 if(length(x) == 0){return(c(0.999,0.999))}
 res = c(mean(x[,1]),mean(rowSums(x[,-1])>=1))
 return(res)
}
{
 suppressMessages(library("tidyverse"))
load("FAB_results.rdata")
 load("surrogate_results.rdata")
 load("A_auxiliary_results.rdata")
 load("B_auxiliary_results.rdata")
 load("bonferroni_results.rdata")
 load("holm_results.rdata")

 sink(file = "table_K6.tex")

cat("\\begin{tabular}{rcccccc} \n")
cat("\\toprule \n")

### SCENARIO 1
tmp_1 = rbind(my_summary(bonferroni_results$H0_omega_1),
              my_summary(A_auxiliary_results$H0_omega_1),
	      my_summary(FAB_results$H0_omega_1), 
              my_summary(surrogate_results$H0_omega_1),
              my_summary(B_auxiliary_results$H0_omega_1),
              my_summary(holm_results$H0_omega_1)
             )

tmp_2 = rbind(my_summary(A_auxiliary_results$H0_omega_2),
              my_summary(bonferroni_results$H0_omega_2),
	      my_summary(FAB_results$H0_omega_2), 
              my_summary(surrogate_results$H0_omega_2),
              my_summary(B_auxiliary_results$H0_omega_2),
              my_summary(holm_results$H0_omega_2)
             )

tmp_10 = rbind(my_summary(A_auxiliary_results$H0_omega_10),
              my_summary(bonferroni_results$H0_omega_10),
	      my_summary(FAB_results$H0_omega_10), 
              my_summary(surrogate_results$H0_omega_10),
              my_summary(B_auxiliary_results$H0_omega_10),
              my_summary(holm_results$H0_omega_10)
             )

cat("\\multicolumn{7}{c}{Scenario 1}\\\\ \n")
cat("\\midrule \n")
cat("& \\multicolumn{2}{c}{$ R_{k,c}= 1$} & \\multicolumn{2}{c}{$R_{k,c} = 2$} & \\multicolumn{2}{c}{$R_{k,c} = 10$} \\\\ \n") 
cat("Method  & subgroup 1 & subgroup 2--6 & subgroup 1 & subgroup 2--6 & subgroup 1 & subgroup 2--6 \\\\ \n")
cat("A-auxiliary &")
cat(sprintf("%.3f & %.3f &", tmp_1[2,1], tmp_1[2,2]))
cat(sprintf("%.3f & %.3f &", tmp_2[2,1], tmp_2[2,2]))
cat(sprintf("%.3f & %.3f \\\\ \n", tmp_10[2,1],tmp_10[2,2]))


cat("B-auxiliary &")
cat(sprintf("%.3f & %.3f &", tmp_1[5,1], tmp_1[5,2]))
cat(sprintf("%.3f & %.3f &", tmp_2[5,1], tmp_2[5,2]))
cat(sprintf("%.3f & %.3f \\\\ \n", tmp_10[5,1],tmp_10[5,2]))

cat("Bonferroni &")
cat(sprintf("%.3f & %.3f &", tmp_1[1,1], tmp_1[1,2]))
cat(sprintf("%.3f & %.3f &", tmp_2[1,1], tmp_2[1,2]))
cat(sprintf("%.3f & %.3f \\\\ \n", tmp_10[1,1],tmp_10[1,2]))


cat("Holm &")
cat(sprintf("%.3f & %.3f &", tmp_1[6,1], tmp_1[6,2]))
cat(sprintf("%.3f & %.3f &", tmp_2[6,1], tmp_2[6,2]))
cat(sprintf("%.3f & %.3f \\\\ \n", tmp_10[6,1],tmp_10[6,2]))


cat("FAB &")
cat(sprintf("%.3f & %.3f &", tmp_1[3,1], tmp_1[3,2]))
cat(sprintf("%.3f & %.3f &", tmp_2[3,1], tmp_2[3,2]))
cat(sprintf("%.3f & %.3f \\\\ \n", tmp_10[3,1],tmp_10[3,2]))

cat("Surrogate &")
cat(sprintf("%.3f & %.3f &", tmp_1[4,1], tmp_1[4,2]))
cat(sprintf("%.3f & %.3f &", tmp_2[4,1], tmp_2[4,2]))
cat(sprintf("%.3f & %.3f \\\\ \n", tmp_10[4,1],tmp_10[4,2]))
cat("\\bottomrule \n")

### SCENARIO 2
tmp_1 = rbind(my_summary(bonferroni_results$H01_omega_1),
              my_summary(A_auxiliary_results$H01_omega_1),
              my_summary(FAB_results$H01_omega_1),
              my_summary(surrogate_results$H01_omega_1),
              my_summary(B_auxiliary_results$H01_omega_1),
              my_summary(holm_results$H01_omega_1)
              )

tmp_2 = rbind(my_summary(bonferroni_results$H01_omega_2),
              my_summary(A_auxiliary_results$H01_omega_2),
              my_summary(FAB_results$H01_omega_2),
              my_summary(surrogate_results$H01_omega_2),
              my_summary(B_auxiliary_results$H01_omega_2),
              my_summary(holm_results$H01_omega_2)
              )

tmp_10 = rbind(my_summary(bonferroni_results$H01_omega_10),
              my_summary(A_auxiliary_results$H01_omega_10),
              my_summary(FAB_results$H01_omega_10),
              my_summary(surrogate_results$H01_omega_10),
              my_summary(B_auxiliary_results$H01_omega_10),
              my_summary(holm_results$H01_omega_10)
              )


cat("\\multicolumn{7}{c}{Scenario 2}\\\\ \n")
cat("\\midrule \n")
cat("& \\multicolumn{2}{c}{$ R_{k,c} = 1$} & \\multicolumn{2}{c}{$ R_{k,c} = 2$} & \\multicolumn{2}{c}{$ R_{k,c} = 10$} \\\\") 
cat("Method  & subgroup 1 & subgroup 2--6 & subgroup 1 & subgroup 2--6 & subgroup 1 & subgroup 2--6 \\\\ \n")
cat("A-auxiliary &")
cat(sprintf("%.3f & %.3f &", tmp_1[2,1], tmp_1[2,2]))
cat(sprintf("%.3f & %.3f &", tmp_2[2,1], tmp_2[2,2]))
cat(sprintf("%.3f & %.3f \\\\ \n", tmp_10[2,1],tmp_10[2,2]))


cat("B-auxiliary &")
cat(sprintf("%.3f & %.3f &", tmp_1[5,1], tmp_1[5,2]))
cat(sprintf("%.3f & %.3f &", tmp_2[5,1], tmp_2[5,2]))
cat(sprintf("%.3f & %.3f \\\\ \n", tmp_10[5,1],tmp_10[5,2]))

cat("Bonferroni &")
cat(sprintf("%.3f & %.3f &", tmp_1[1,1], tmp_1[1,2]))
cat(sprintf("%.3f & %.3f &", tmp_2[1,1], tmp_2[1,2]))
cat(sprintf("%.3f & %.3f \\\\ \n", tmp_10[1,1],tmp_10[1,2]))

cat("Holm &")
cat(sprintf("%.3f & %.3f &", tmp_1[6,1], tmp_1[6,2]))
cat(sprintf("%.3f & %.3f &", tmp_2[6,1], tmp_2[6,2]))
cat(sprintf("%.3f & %.3f \\\\ \n", tmp_10[6,1],tmp_10[6,2]))


cat("FAB &")
cat(sprintf("%.3f & %.3f &", tmp_1[3,1], tmp_1[3,2]))
cat(sprintf("%.3f & %.3f &", tmp_2[3,1], tmp_2[3,2]))
cat(sprintf("%.3f & %.3f \\\\ \n", tmp_10[3,1],tmp_10[3,2]))


cat("Surrogate &")
cat(sprintf("%.3f & %.3f &", tmp_1[4,1], tmp_1[4,2]))
cat(sprintf("%.3f & %.3f &", tmp_2[4,1], tmp_2[4,2]))
cat(sprintf("%.3f & %.3f \\\\ \n", tmp_10[4,1],tmp_10[4,2]))
cat("\\bottomrule \n")


tmp_1 = rbind(my_summary(bonferroni_results$H10_omega_1),
              my_summary(A_auxiliary_results$H10_omega_1),
              my_summary(FAB_results$H10_omega_1),
              my_summary(surrogate_results$H10_omega_1),
              my_summary(B_auxiliary_results$H10_omega_1),
              my_summary(holm_results$H10_omega_1)
              )


tmp_2 = rbind(my_summary(bonferroni_results$H10_omega_2),
              my_summary(A_auxiliary_results$H10_omega_2),
              my_summary(FAB_results$H10_omega_2),
              my_summary(surrogate_results$H10_omega_2),
              my_summary(B_auxiliary_results$H10_omega_2),
              my_summary(holm_results$H10_omega_2)
              )

tmp_10 = rbind(my_summary(bonferroni_results$H10_omega_10),
              my_summary(A_auxiliary_results$H10_omega_10),
              my_summary(FAB_results$H10_omega_10),
              my_summary(surrogate_results$H10_omega_10),
              my_summary(B_auxiliary_results$H10_omega_10),
              my_summary(holm_results$H10_omega_10)
              )

cat("\\multicolumn{7}{c}{Scenario 3}\\\\ \n")
cat("\\midrule \n")
cat("& \\multicolumn{2}{c}{$ R_{k,c} = 1$} & \\multicolumn{2}{c}{$R_{k,c} = 2$} & \\multicolumn{2}{c}{$R_{k,c} = 10$} \\\\ \n") 
cat("Method  & subgroup 1 & subgroup 2--6 & subgroup 1 & subgroup 2--6 & subgroup 1 & subgroup 2--6 \\\\ \n")
cat("A-auxiliary &")
cat(sprintf("%.3f & %.3f &", tmp_1[2,1], tmp_1[2,2]))
cat(sprintf("%.3f & %.3f &", tmp_2[2,1], tmp_2[2,2]))
cat(sprintf("%.3f & %.3f \\\\ \n ", tmp_10[2,1],tmp_10[2,2]))

cat("B-auxiliary &")
cat(sprintf("%.3f & %.3f &", tmp_1[5,1], tmp_1[5,2]))
cat(sprintf("%.3f & %.3f &", tmp_2[5,1], tmp_2[5,2]))
cat(sprintf("%.3f & %.3f \\\\ \n ", tmp_10[5,1],tmp_10[5,2]))

cat("Bonferroni &")
cat(sprintf("%.3f & %.3f &", tmp_1[1,1], tmp_1[1,2]))
cat(sprintf("%.3f & %.3f &", tmp_2[1,1], tmp_2[1,2]))
cat(sprintf("%.3f & %.3f \\\\ \n", tmp_10[1,1],tmp_10[1,2]))

cat("Holm &")
cat(sprintf("%.3f & %.3f &", tmp_1[6,1], tmp_1[6,2]))
cat(sprintf("%.3f & %.3f &", tmp_2[6,1], tmp_2[6,2]))
cat(sprintf("%.3f & %.3f \\\\ \n", tmp_10[6,1],tmp_10[6,2]))

cat("FAB &")
cat(sprintf("%.3f & %.3f &", tmp_1[3,1], tmp_1[3,2]))
cat(sprintf("%.3f & %.3f &", tmp_2[3,1], tmp_2[3,2]))
cat(sprintf("%.3f & %.3f \\\\ \n", tmp_10[3,1],tmp_10[3,2]))


cat("Surrogate &")
cat(sprintf("%.3f & %.3f &", tmp_1[4,1], tmp_1[4,2]))
cat(sprintf("%.3f & %.3f &", tmp_2[4,1], tmp_2[4,2]))
cat(sprintf("%.3f & %.3f \\\\ \n", tmp_10[4,1],tmp_10[4,2]))

cat("\\bottomrule \n")

## SCENARIO 4
tmp_1 = rbind(my_summary(bonferroni_results$H11_omega_1),
              my_summary(A_auxiliary_results$H11_omega_1),
              my_summary(FAB_results$H11_omega_1),
              my_summary(surrogate_results$H11_omega_1),
              my_summary(B_auxiliary_results$H11_omega_1),
              my_summary(holm_results$H11_omega_1)
             )

tmp_2 = rbind(my_summary(bonferroni_results$H11_omega_2),
              my_summary(A_auxiliary_results$H11_omega_2),
              my_summary(FAB_results$H11_omega_2),
              my_summary(surrogate_results$H11_omega_2),
              my_summary(B_auxiliary_results$H11_omega_2),
              my_summary(holm_results$H11_omega_2)
             )

tmp_10 = rbind(my_summary(bonferroni_results$H11_omega_10),
               my_summary(A_auxiliary_results$H11_omega_10),
               my_summary(FAB_results$H11_omega_10),
               my_summary(surrogate_results$H11_omega_10),
               my_summary(B_auxiliary_results$H11_omega_10),
               my_summary(holm_results$H11_omega_10)
              )




cat("\\multicolumn{7}{c}{Scenario 4}\\\\ \n")
cat("\\midrule \n")
cat("& \\multicolumn{2}{c}{$R_{k,c} = 1$} & \\multicolumn{2}{c}{$R_{k,c} = 2$} & \\multicolumn{2}{c}{$R_{k,c} = 10$} \\\\ \n") 
cat("Method  & subgroup 1 & subgroup 2--6 & subgroup 1 & subgroup 2--6 & subgroup 1 & subgroup 2--6 \\\\ \n")
cat("A-auxiliary &")
cat(sprintf("%.3f & %.3f &", tmp_1[2,1], tmp_1[2,2]))
cat(sprintf("%.3f & %.3f &", tmp_2[2,1], tmp_2[2,2]))
cat(sprintf("%.3f & %.3f \\\\ \n ", tmp_10[2,1],tmp_10[2,2]))


cat("B-auxiliary &")
cat(sprintf("%.3f & %.3f &", tmp_1[5,1], tmp_1[5,2]))
cat(sprintf("%.3f & %.3f &", tmp_2[5,1], tmp_2[5,2]))
cat(sprintf("%.3f & %.3f \\\\ \n ", tmp_10[5,1],tmp_10[5,2]))


cat("Bonferroni &")
cat(sprintf("%.3f & %.3f &", tmp_1[1,1], tmp_1[1,2]))
cat(sprintf("%.3f & %.3f &", tmp_2[1,1], tmp_2[1,2]))
cat(sprintf("%.3f & %.3f \\\\ \n", tmp_10[1,1],tmp_10[1,2]))

cat("Holm &")
cat(sprintf("%.3f & %.3f &", tmp_1[6,1], tmp_1[6,2]))
cat(sprintf("%.3f & %.3f &", tmp_2[6,1], tmp_2[6,2]))
cat(sprintf("%.3f & %.3f \\\\ \n", tmp_10[6,1],tmp_10[6,2]))

cat("FAB &")
cat(sprintf("%.3f & %.3f &", tmp_1[3,1], tmp_1[3,2]))
cat(sprintf("%.3f & %.3f &", tmp_2[3,1], tmp_2[3,2]))
cat(sprintf("%.3f & %.3f \\\\ \n", tmp_10[3,1],tmp_10[3,2]))


cat("Surrogate &")
cat(sprintf("%.3f & %.3f &", tmp_1[4,1], tmp_1[4,2]))
cat(sprintf("%.3f & %.3f &", tmp_2[4,1], tmp_2[4,2]))
cat(sprintf("%.3f & %.3f \\\\ \n", tmp_10[4,1],tmp_10[4,2]))
cat("\\bottomrule")

## SCENARIO 5
tmp_1 = rbind(my_summary(bonferroni_results$Hmiss_omega_1),
              my_summary(A_auxiliary_results$Hmiss_omega_1),
              my_summary(FAB_results$Hmiss_omega_1),
              my_summary(surrogate_results$Hmiss_omega_1),
              my_summary(B_auxiliary_results$Hmiss_omega_1),
              my_summary(holm_results$Hmiss_omega_1)
              )


tmp_2 = rbind(my_summary(bonferroni_results$Hmiss_omega_2),
              my_summary(A_auxiliary_results$Hmiss_omega_2),
              my_summary(FAB_results$Hmiss_omega_2),
              my_summary(surrogate_results$Hmiss_omega_2),
              my_summary(B_auxiliary_results$Hmiss_omega_2),
              my_summary(holm_results$Hmiss_omega_2)
              )

tmp_10 = rbind(my_summary(bonferroni_results$Hmiss_omega_10),
              my_summary(A_auxiliary_results$Hmiss_omega_10),
              my_summary(FAB_results$Hmiss_omega_10),
              my_summary(surrogate_results$Hmiss_omega_10),
              my_summary(B_auxiliary_results$Hmiss_omega_10),
              my_summary(holm_results$Hmiss_omega_10)
              )

cat("\\multicolumn{7}{c}{Scenario 5}\\\\ \n")
cat("\\midrule \n")
cat("& \\multicolumn{2}{c}{$R_{k,c} = 1$} & \\multicolumn{2}{c}{$R_{k,c} = 2$} & \\multicolumn{2}{c}{$R_{k,c} = 10$} \\\\ \n") 
cat("Method  & subgroup 1 & subgroup 2--6 & subgroup 1 & subgroup 2--6 & subgroup 1 & subgroup 2--6 \\\\ \n")
cat("A-auxiliary &")
cat(sprintf("%.3f & %.3f &", tmp_1[2,1], tmp_1[2,2]))
cat(sprintf("%.3f & %.3f &", tmp_2[2,1], tmp_2[2,2]))
cat(sprintf("%.3f & %.3f \\\\ \n ", tmp_10[2,1],tmp_10[2,2]))


cat("B-auxiliary &")
cat(sprintf("%.3f & %.3f &", tmp_1[5,1], tmp_1[5,2]))
cat(sprintf("%.3f & %.3f &", tmp_2[5,1], tmp_2[5,2]))
cat(sprintf("%.3f & %.3f \\\\ \n ", tmp_10[5,1],tmp_10[5,2]))


cat("Bonferroni &")
cat(sprintf("%.3f & %.3f &", tmp_1[1,1], tmp_1[1,2]))
cat(sprintf("%.3f & %.3f &", tmp_2[1,1], tmp_2[1,2]))
cat(sprintf("%.3f & %.3f \\\\ \n", tmp_10[1,1],tmp_10[1,2]))


cat("Holm &")
cat(sprintf("%.3f & %.3f &", tmp_1[6,1], tmp_1[6,2]))
cat(sprintf("%.3f & %.3f &", tmp_2[6,1], tmp_2[6,2]))
cat(sprintf("%.3f & %.3f \\\\ \n", tmp_10[6,1],tmp_10[6,2]))



cat("FAB &")
cat(sprintf("%.3f & %.3f &", tmp_1[3,1], tmp_1[3,2]))
cat(sprintf("%.3f & %.3f &", tmp_2[3,1], tmp_2[3,2]))
cat(sprintf("%.3f & %.3f \\\\ \n", tmp_10[3,1],tmp_10[3,2]))


cat("Surrogate &")
cat(sprintf("%.3f & %.3f &", tmp_1[4,1], tmp_1[4,2]))
cat(sprintf("%.3f & %.3f &", tmp_2[4,1], tmp_2[4,2]))
cat(sprintf("%.3f & %.3f \\\\ \n", tmp_10[4,1],tmp_10[4,2]))
cat("\\bottomrule \n")


cat("\\end{tabular}")
sink()
}
