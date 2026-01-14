{
 suppressMessages(library("tidyverse"))
 load("FAB_results.rdata")
 load("surrogate_results.rdata")
 load("A_auxiliary_results.rdata")
 load("B_auxiliary_results.rdata")
 load("bonferroni_results.rdata")
 load("holm_results.rdata")

 sink(file = "table.tex")

cat("\\begin{tabular}{rcccccc} \n")
cat("\\toprule \n")

### SCENARIO 1
tmp_1 = rbind(colMeans(bonferroni_results$H0_omega_1),
              colMeans(A_auxiliary_results$H0_omega_1),
	      colMeans(FAB_results$H0_omega_1), 
              colMeans(surrogate_results$H0_omega_1),
              colMeans(B_auxiliary_results$H0_omega_1),
              colMeans(holm_results$H0_omega_1)
             )

tmp_2 = rbind(
              colMeans(bonferroni_results$H0_omega_2),
	      colMeans(A_auxiliary_results$H0_omega_2),
	      colMeans(FAB_results$H0_omega_2), 
              colMeans(surrogate_results$H0_omega_2),
              colMeans(B_auxiliary_results$H0_omega_2),
              colMeans(holm_results$H0_omega_2)
             )

tmp_10 = rbind(
              colMeans(bonferroni_results$H0_omega_10),
	       colMeans(A_auxiliary_results$H0_omega_10),
	      colMeans(FAB_results$H0_omega_10), 
              colMeans(surrogate_results$H0_omega_10),
              colMeans(B_auxiliary_results$H0_omega_10),
              colMeans(holm_results$H0_omega_10)
             )

cat("\\multicolumn{7}{c}{Scenario 1}\\\\ \n")
cat("\\midrule \n")
cat("& \\multicolumn{2}{c}{$ R_{k,c}= 1$} & \\multicolumn{2}{c}{$R_{k,c} = 2$} & \\multicolumn{2}{c}{$R_{k,c} = 10$} \\\\ \n") 
cat("Method  & subgroup 1 & subgroup 2 & subgroup 1 & subgroup 2 & subgroup 1 & subgroup 2 \\\\ \n")
cat("Auxiliary-Augmented &")
cat(sprintf("%.3f & %.3f &", tmp_1[2,1], tmp_1[2,2]))
cat(sprintf("%.3f & %.3f &", tmp_2[2,1], tmp_2[2,2]))
cat(sprintf("%.3f & %.3f \\\\ \n", tmp_10[2,1],tmp_10[2,2]))


cat("Auxiliary-Augmented-B &")
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

cat("Auxiliary-Only &")
cat(sprintf("%.3f & %.3f &", tmp_1[4,1], tmp_1[4,2]))
cat(sprintf("%.3f & %.3f &", tmp_2[4,1], tmp_2[4,2]))
cat(sprintf("%.3f & %.3f \\\\ \n", tmp_10[4,1],tmp_10[4,2]))
cat("\\bottomrule \n")

### SCENARIO 2
tmp_1 = rbind(colMeans(bonferroni_results$H01_omega_1),
              colMeans(A_auxiliary_results$H01_omega_1),
              colMeans(FAB_results$H01_omega_1),
              colMeans(surrogate_results$H01_omega_1),
              colMeans(B_auxiliary_results$H01_omega_1),
              colMeans(holm_results$H01_omega_1)
              )

tmp_2 = rbind(colMeans(bonferroni_results$H01_omega_2),
              colMeans(A_auxiliary_results$H01_omega_2),
              colMeans(FAB_results$H01_omega_2),
              colMeans(surrogate_results$H01_omega_2),
              colMeans(B_auxiliary_results$H01_omega_2),
              colMeans(holm_results$H01_omega_2)
              )

tmp_10 = rbind(colMeans(bonferroni_results$H01_omega_10),
              colMeans(A_auxiliary_results$H01_omega_10),
              colMeans(FAB_results$H01_omega_10),
              colMeans(surrogate_results$H01_omega_10),
              colMeans(B_auxiliary_results$H01_omega_10),
              colMeans(holm_results$H01_omega_10)
              )


cat("\\multicolumn{7}{c}{Scenario 2}\\\\ \n")
cat("\\midrule \n")
cat("& \\multicolumn{2}{c}{$ R_{k,c} = 1$} & \\multicolumn{2}{c}{$ R_{k,c} = 2$} & \\multicolumn{2}{c}{$ R_{k,c} = 10$} \\\\") 
cat("Method  & subgroup 1 & subgroup 2 & subgroup 1 & subgroup 2 & subgroup 1 & subgroup 2 \\\\ \n")
cat("Auxiliary-Augmented &")
cat(sprintf("%.3f & %.3f &", tmp_1[2,1], tmp_1[2,2]))
cat(sprintf("%.3f & %.3f &", tmp_2[2,1], tmp_2[2,2]))
cat(sprintf("%.3f & %.3f \\\\ \n", tmp_10[2,1],tmp_10[2,2]))


cat("Auxiliary-Augmented-B &")
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


cat("Auxiliary-Only &")
cat(sprintf("%.3f & %.3f &", tmp_1[4,1], tmp_1[4,2]))
cat(sprintf("%.3f & %.3f &", tmp_2[4,1], tmp_2[4,2]))
cat(sprintf("%.3f & %.3f \\\\ \n", tmp_10[4,1],tmp_10[4,2]))
cat("\\bottomrule \n")

colMeans(A_auxiliary_results$H10_omega_1)

tmp_1 = rbind(colMeans(bonferroni_results$H10_omega_1),
              colMeans(A_auxiliary_results$H10_omega_1),
              colMeans(FAB_results$H10_omega_1),
              colMeans(surrogate_results$H10_omega_1),
              colMeans(B_auxiliary_results$H10_omega_1),
              colMeans(holm_results$H10_omega_1)
              )


tmp_2 = rbind(colMeans(bonferroni_results$H10_omega_2),
              colMeans(A_auxiliary_results$H10_omega_2),
              apply(FAB_results$H10_omega_2,2,mean, na.rm = TRUE),
              colMeans(surrogate_results$H10_omega_2),
              colMeans(B_auxiliary_results$H10_omega_2),
              colMeans(holm_results$H10_omega_2)
              )

tmp_10 = rbind(colMeans(bonferroni_results$H10_omega_10),
              colMeans(A_auxiliary_results$H10_omega_10),
              colMeans(FAB_results$H10_omega_10),
              colMeans(surrogate_results$H10_omega_10),
              colMeans(B_auxiliary_results$H10_omega_10),
              colMeans(holm_results$H10_omega_10)
              )

cat("\\multicolumn{7}{c}{Scenario 3}\\\\ \n")
cat("\\midrule \n")
cat("& \\multicolumn{2}{c}{$ R_{k,c} = 1$} & \\multicolumn{2}{c}{$R_{k,c} = 2$} & \\multicolumn{2}{c}{$R_{k,c} = 10$} \\\\ \n") 
cat("Method  & subgroup 1 & subgroup 2 & subgroup 1 & subgroup 2 & subgroup 1 & subgroup 2 \\\\ \n")
cat("Auxiliary-Augmented &")
cat(sprintf("%.3f & %.3f &", tmp_1[2,1], tmp_1[2,2]))
cat(sprintf("%.3f & %.3f &", tmp_2[2,1], tmp_2[2,2]))
cat(sprintf("%.3f & %.3f \\\\ \n ", tmp_10[2,1],tmp_10[2,2]))

cat("Auxiliary-Augmented-B &")
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


cat("Auxiliary-Only &")
cat(sprintf("%.3f & %.3f &", tmp_1[4,1], tmp_1[4,2]))
cat(sprintf("%.3f & %.3f &", tmp_2[4,1], tmp_2[4,2]))
cat(sprintf("%.3f & %.3f \\\\ \n", tmp_10[4,1],tmp_10[4,2]))

cat("\\bottomrule \n")

## SCENARIO 4
tmp_1 = rbind(colMeans(bonferroni_results$H11_omega_1),
              colMeans(A_auxiliary_results$H11_omega_1),
              colMeans(FAB_results$H11_omega_1),
              colMeans(surrogate_results$H11_omega_1),
              colMeans(B_auxiliary_results$H11_omega_1),
              colMeans(holm_results$H11_omega_1)
             )

tmp_2 = rbind(colMeans(bonferroni_results$H11_omega_2),
              colMeans(A_auxiliary_results$H11_omega_2),
              colMeans(FAB_results$H11_omega_2),
              colMeans(surrogate_results$H11_omega_2),
              colMeans(B_auxiliary_results$H11_omega_2),
              colMeans(holm_results$H11_omega_2)
             )

tmp_10 = rbind(colMeans(bonferroni_results$H11_omega_10),
               colMeans(A_auxiliary_results$H11_omega_10),
               colMeans(FAB_results$H11_omega_10),
               colMeans(surrogate_results$H11_omega_10),
               colMeans(B_auxiliary_results$H11_omega_10),
               colMeans(holm_results$H11_omega_10)
              )




cat("\\multicolumn{7}{c}{Scenario 4}\\\\ \n")
cat("\\midrule \n")
cat("& \\multicolumn{2}{c}{$R_{k,c} = 1$} & \\multicolumn{2}{c}{$R_{k,c} = 2$} & \\multicolumn{2}{c}{$R_{k,c} = 10$} \\\\ \n") 
cat("Method  & subgroup 1 & subgroup 2 & subgroup 1 & subgroup 2 & subgroup 1 & subgroup 2 \\\\ \n")
cat("Auxiliary-Augmented &")
cat(sprintf("%.3f & %.3f &", tmp_1[2,1], tmp_1[2,2]))
cat(sprintf("%.3f & %.3f &", tmp_2[2,1], tmp_2[2,2]))
cat(sprintf("%.3f & %.3f \\\\ \n ", tmp_10[2,1],tmp_10[2,2]))


cat("Auxiliary-Augmented-B &")
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


cat("Auxiliary-Only &")
cat(sprintf("%.3f & %.3f &", tmp_1[4,1], tmp_1[4,2]))
cat(sprintf("%.3f & %.3f &", tmp_2[4,1], tmp_2[4,2]))
cat(sprintf("%.3f & %.3f \\\\ \n", tmp_10[4,1],tmp_10[4,2]))
cat("\\bottomrule")

## SCENARIO 5
tmp_1 = rbind(colMeans(bonferroni_results$Hmiss_omega_1),
              colMeans(A_auxiliary_results$Hmiss_omega_1),
              colMeans(FAB_results$Hmiss_omega_1),
              colMeans(surrogate_results$Hmiss_omega_1),
              colMeans(B_auxiliary_results$Hmiss_omega_1),
              colMeans(holm_results$Hmiss_omega_1)
              )


tmp_2 = rbind(colMeans(bonferroni_results$Hmiss_omega_2),
              colMeans(A_auxiliary_results$Hmiss_omega_2),
              colMeans(FAB_results$Hmiss_omega_2),
              colMeans(surrogate_results$Hmiss_omega_2),
              colMeans(B_auxiliary_results$Hmiss_omega_2),
              colMeans(holm_results$Hmiss_omega_2)
              )

tmp_10 = rbind(colMeans(bonferroni_results$Hmiss_omega_10),
              colMeans(A_auxiliary_results$Hmiss_omega_10),
              colMeans(FAB_results$Hmiss_omega_10),
              colMeans(surrogate_results$Hmiss_omega_10),
              colMeans(B_auxiliary_results$Hmiss_omega_10),
              colMeans(holm_results$Hmiss_omega_10)
              )

cat("\\multicolumn{7}{c}{Scenario 5}\\\\ \n")
cat("\\midrule \n")
cat("& \\multicolumn{2}{c}{$R_{k,c} = 1$} & \\multicolumn{2}{c}{$R_{k,c} = 2$} & \\multicolumn{2}{c}{$R_{k,c} = 10$} \\\\ \n") 
cat("Method  & subgroup 1 & subgroup 2 & subgroup 1 & subgroup 2 & subgroup 1 & subgroup 2 \\\\ \n")
cat("Auxiliary-Augmented &")
cat(sprintf("%.3f & %.3f &", tmp_1[2,1], tmp_1[2,2]))
cat(sprintf("%.3f & %.3f &", tmp_2[2,1], tmp_2[2,2]))
cat(sprintf("%.3f & %.3f \\\\ \n ", tmp_10[2,1],tmp_10[2,2]))


cat("Auxiliary-Augmented-B &")
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


cat("Auxiliary-Only &")
cat(sprintf("%.3f & %.3f &", tmp_1[4,1], tmp_1[4,2]))
cat(sprintf("%.3f & %.3f &", tmp_2[4,1], tmp_2[4,2]))
cat(sprintf("%.3f & %.3f \\\\ \n", tmp_10[4,1],tmp_10[4,2]))
cat("\\bottomrule \n")


cat("\\end{tabular}")
sink()
}
