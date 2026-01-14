 suppressMessages(library("tidyverse"))
 load("FAB_results.rdata")
 load("surrogate_results.rdata")
 load("A_auxiliary_results.rdata")
 load("B_auxiliary_results.rdata")
 load("bonferroni_results.rdata")
 load("holm_results.rdata")

FWER = function(x)
{
 mean(rowSums(x)>=1)
}

## SCENARIO 1
FWER_1 = rbind(FWER(bonferroni_results$H0_omega_1),
              FWER(A_auxiliary_results$H0_omega_1),
	      FWER(FAB_results$H0_omega_1), 
              FWER(surrogate_results$H0_omega_1),
              FWER(B_auxiliary_results$H0_omega_1),
              FWER(holm_results$H0_omega_1)
             )

FWER_2 = rbind(FWER(bonferroni_results$H0_omega_2),
              FWER(A_auxiliary_results$H0_omega_2),
	      FWER(FAB_results$H0_omega_2), 
              FWER(surrogate_results$H0_omega_2),
              FWER(B_auxiliary_results$H0_omega_2),
              FWER(holm_results$H0_omega_2)
             )


FWER_10 = rbind(FWER(bonferroni_results$H0_omega_10),
              FWER(A_auxiliary_results$H0_omega_10),
	      FWER(FAB_results$H0_omega_10), 
              FWER(surrogate_results$H0_omega_10),
              FWER(B_auxiliary_results$H0_omega_10),
              FWER(holm_results$H0_omega_10)
             )

row.names(FWER_1) = row.names(FWER_2) = row.names(FWER_10) = 
	c("Bonferroni", "A-auxiliary","FAB", "Surrogate", "B-auxiliary", "Holm")


Scenario1 = cbind(FWER_1,FWER_2,FWER_10)

## SCENARIO 2
rm(list = c('FWER_1','FWER_2','FWER_10'))

FWER_1 = rbind(FWER(bonferroni_results$H01_omega_1),
              FWER(A_auxiliary_results$H01_omega_1),
              FWER(FAB_results$H01_omega_1),
              FWER(surrogate_results$H01_omega_1),
              FWER(B_auxiliary_results$H01_omega_1),
              FWER(holm_results$H01_omega_1)
              )

FWER_2 = rbind(FWER(bonferroni_results$H01_omega_2),
              FWER(A_auxiliary_results$H01_omega_2),
              FWER(FAB_results$H01_omega_2),
              FWER(surrogate_results$H01_omega_2),
              FWER(B_auxiliary_results$H01_omega_2),
              FWER(holm_results$H01_omega_2)
              )

FWER_10 = rbind(FWER(bonferroni_results$H01_omega_10),
              FWER(A_auxiliary_results$H01_omega_10),
              FWER(FAB_results$H01_omega_10),
              FWER(surrogate_results$H01_omega_10),
              FWER(B_auxiliary_results$H01_omega_10),
              FWER(holm_results$H01_omega_10)
              )

row.names(FWER_1) = row.names(FWER_2) = row.names(FWER_10) = 
	c("Bonferroni", "A-auxiliary","FAB", "Surrogate", "B-auxiliary", "Holm")

Scenario2 = cbind(FWER_1,FWER_2,FWER_10)



tab = cbind(Scenario1, Scenario2)
tab = tab[c(2,5,1,6,3,4),]
row.names(tab) = c("Auxilary-Augmented", "Auxilary-Augmented-B", "Bonferroni", "Holm", "FAB", "Auxiliary-Only")


{
sink(file = "FWER_K6.tex")
cat("\\begin{tabular}{rccc|ccc} \n")
cat("\\toprule \n")

cat("& \\multicolumn{3}{c}{Scenario 1} & \\multicolumn{3}{c}{Scenario 2} \\\\ \n")
cat("\\midrule \n")

cat("Method  & $ R_{k,c}= 1$ & $R_{k,c} = 2$ & $R_{k,c} = 10$  & $ R_{k,c}= 1$ & $R_{k,c} = 2$ & $R_{k,c} = 10$ \\\\ \n") 


my_format_string = "%s & %.3f & %.3f &%.3f &%.3f &%.3f &%.3f \\\\ \n" 
for(i in 1:NROW(tab)){
tmp = list()
tmp[[1]] = row.names(tab)[i]
for(j in 2:(NCOL(tab)+1)) tmp[[j]] = tab[i,j-1]
cat(do.call(sprintf, c(my_format_string, tmp)))
}
cat("\\bottomrule \n")
cat("\\end{tabular}")
sink()
}

