library(dplyr)
load("A_auxiliary_results_prior_sensitivity.rdata")
tab_row_text = paste(c(rep("%.3f &",5), "%.3f \\\\"), collapse = '')



res = lapply(A_auxiliary_results,  colMeans)
sink("sensitivity_prior_table.tex")
cat("
\\begin{tabular}{rccccc}
\\toprule 
\n")
cat("& \\multicolumn{2}{c}{$R_{k,c} = 1$} & \\multicolumn{2}{c}{$R_{k,c} = 2$} & \\multicolumn{2}{c}{$R_{k,c} = 10$} \\\\")
	cat("Scenario 1 &",
do.call(sprintf,c(fmt = tab_row_text, as.list(unlist(res[1:3])))))

	cat("Scenario 2 &", do.call(sprintf,c(fmt = tab_row_text, as.list(unlist(res[4:6])))))

	cat("Scenario 3 &", do.call(sprintf,c(fmt = tab_row_text, as.list(unlist(res[7:9])))))

	cat("Scenario 4 &", do.call(sprintf,c(fmt = tab_row_text, as.list(unlist(res[10:12])))))

	cat("Scenario 5 &", do.call(sprintf,c(fmt = tab_row_text, as.list(unlist(res[13:15])))))
cat("\\bottomrule  \n")
cat("
\\end{tabular}
\n ")
sink()
