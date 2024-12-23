library(dplyr)
load("A_auxiliary_results_sensitivity.rdata")
tab_row_text = paste(c(rep("%.3f &",9), "%.3f \\\\"), collapse = '')

scenarios_names = 
c("Scenario 1 $R_{k,c} = 1$",
"Scenario 1 $R_{k,c} = 2$",
"Scenario 1 $R_{k,c} = 10$",
"Scenario 2 $R_{k,c} = 1$",
"Scenario 2 $R_{k,c} = 2$",
"Scenario 2 $R_{k,c} = 10$",
"Scenario 3 $R_{k,c} = 1$",
"Scenario 3 $R_{k,c} = 2$",
"Scenario 3 $R_{k,c} = 10$",
"Scenario 4 $R_{k,c} = 1$",
"Scenario 4 $R_{k,c} = 2$",
"Scenario 4 $R_{k,c} = 10$",
"Scenario 5 $R_{k,c} = 1$",
"Scenario 5 $R_{k,c} = 2$",
"Scenario 5 $R_{k,c} = 10$")

optimal_solutions = readRDS("optimal_solutions.rds")
#names = optimal_solutions |> mutate(names  = sprintf( "$\\lambda =$%.2f, $\\beta = $%.2f ",lambda, beta)) |> pull(names)
names = optimal_solutions |> mutate(names  = sprintf( "(%.1f, %.2f)",lambda, beta)) |> pull(names)

res = lapply(A_auxiliary_results, \(x) apply(x,c(2,3),mean))
sink("sensitivity_table.tex")
cat("
\\begin{tabular}{rcccc>{\\columncolor[gray]{0.9}}cccccc}
\\toprule 
\n")
for(l in 1:length(res))
{
cat(sprintf("\\multicolumn{10}{c}{%s}\\\\ \n", scenarios_names[l]))
 cat("\\midrule \n")
 cat("($\\lambda, \\beta_{YS})$ &", paste(names, collapse = "&"), "\\\\ \n")
 cat("subgroup 1 &")
 cat(do.call(sprintf, c(fmt = tab_row_text, as.list(res[[l]][1,]))),"\n")

cat("subgroup 2 &")
 cat(do.call(sprintf, c(fmt = tab_row_text, as.list(res[[l]][2,]))),"\n")
cat("\\bottomrule  \n")
}
cat("
\\end{tabular}
\n ")
sink()
