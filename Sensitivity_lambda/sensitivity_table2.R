library(dplyr)
load("A_auxiliary_results_sensitivity.rdata")
tab_row_text = paste(c(rep("%.3f &",5), "%.3f \\\\"), collapse = '')


optimal_solutions = readRDS("optimal_solutions.rds")
names = optimal_solutions |> mutate(names  = sprintf( "$\\lambda =$%.2f, $\\beta = $%.2f ",lambda, beta)) |> pull(names)





res = lapply(A_auxiliary_results, \(x) apply(x,c(2,3),mean))

res = lapply(res, \(x) matrix(x, nrow = 10, byrow = T))

sink("sensitivity_table.tex")
cat("
\\begin{tabular}{rcccccc} 
\\toprule 
\n")
## Scenario 1
cat("\\multicolumn{7}{c}{Scenario 1}\\\\ \n")
cat("\\midrule \n")
cat("& \\multicolumn{2}{c}{$ R_{k,c}= 1$} & \\multicolumn{2}{c}{$R_{k,c} = 2$} & \\multicolumn{2}{c}{$R_{k,c} = 10$} \\\\ ")
tmp = do.call("cbind",res[1:3])
for(l in 1:10)
{
 cat(names[l],"&",do.call(sprintf, c(fmt = tab_row_text, as.list(tmp[l,]))),"\n")
}
cat("\\bottomrule \n")
## Scenario 2
cat("\\multicolumn{7}{c}{Scenario 2}\\\\ \n")
cat("\\midrule \n")
cat("& \\multicolumn{2}{c}{$ R_{k,c}= 1$} & \\multicolumn{2}{c}{$R_{k,c} = 2$} & \\multicolumn{2}{c}{$R_{k,c} = 10$} \\\\ ")
tmp = do.call("cbind",res[4:6])
for(l in 1:10)
{
 cat(names[l],"&",do.call(sprintf, c(fmt = tab_row_text, as.list(tmp[l,]))),"\n")
}
cat("\\bottomrule \n")

## Scenario 3
cat("\\multicolumn{7}{c}{Scenario 3}\\\\ \n")
cat("\\midrule \n")
cat("& \\multicolumn{2}{c}{$ R_{k,c}= 1$} & \\multicolumn{2}{c}{$R_{k,c} = 2$} & \\multicolumn{2}{c}{$R_{k,c} = 10$} \\\\ ")
tmp = do.call("cbind",res[7:9])
for(l in 1:10)
{
 cat(names[l],"&",do.call(sprintf, c(fmt = tab_row_text, as.list(tmp[l,]))),"\n")
}
cat("\\bottomrule \n")

## Scenario 4
cat("\\multicolumn{7}{c}{Scenario 4}\\\\ \n")
cat("\\midrule \n")
cat("& \\multicolumn{2}{c}{$ R_{k,c}= 1$} & \\multicolumn{2}{c}{$R_{k,c} = 2$} & \\multicolumn{2}{c}{$R_{k,c} = 10$} \\\\ ")
tmp = do.call("cbind",res[10:12])
for(l in 1:10)
{
 cat(names[l],"&",do.call(sprintf, c(fmt = tab_row_text, as.list(tmp[l,]))),"\n")
}
cat("\\bottomrule \n")
## Scenario 5
cat("\\multicolumn{7}{c}{Scenario 5}\\\\ \n")
cat("\\midrule \n")
cat("& \\multicolumn{2}{c}{$ R_{k,c}= 1$} & \\multicolumn{2}{c}{$R_{k,c} = 2$} & \\multicolumn{2}{c}{$R_{k,c} = 10$} \\\\ ")
tmp = do.call("cbind",res[13:15])
for(l in 1:10)
{
 cat(names[l],"&",do.call(sprintf, c(fmt = tab_row_text, as.list(tmp[l,]))),"\n")
}
cat("\\bottomrule \n")


cat("
\\bottomrule 
\\end{tabular}
\n ")
sink()
