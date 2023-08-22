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


cbind(FWER_1,FWER_2,FWER_10)

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


cbind(FWER_1,FWER_2,FWER_10)
