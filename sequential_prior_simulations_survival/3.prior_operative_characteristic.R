library(dplyr)
library(survival)

fileList = list.files("data/")
dataList = lapply(fileList, \(x) readRDS(paste0("data/",x)))

##++++++++++++++++++++++++++++++++++++++++
## Prior trials summaries
##++++++++++++++++++++++++++++++++++++++++
## time in days to observe 50 OS events
t_IA = sapply(dataList, \(x) x$t_ia)
## correlation between OS and PFS
YS_cor = sapply(dataList, \(x) x$dat |> filter(os_status == 1, pfs_status==1) |>summarize(cor = cor(os,pfs)) |> pull(cor))
## number of uncerored OS
OS_uncens = sapply(dataList, \(x)  mean(x$dat$os_status))
## number of uncerored PFS
PFS_uncens = sapply(dataList, \(x) mean(x$dat$pfs_status))

## median OS
tmp = sapply(dataList, \(x) summary(survfit(Surv(os,os_status)~treatment,data = x$dat))$table[,7])

median_OS_treat = tmp[2,]
median_OS_contr = tmp[1,]


## median PFS
tmp = sapply(dataList, \(x) summary(survfit(Surv(pfs,pfs_status)~treatment,data = x$dat))$table[,7])

median_PFS_treat = tmp[2,]
median_PFS_contr = tmp[1,]


latex_summary = function(x)
{
	m    = numeric(7)
	m[1] = mean(x)
        m[2] = sd(x)
	m[3] = min(x)
	m[4] = quantile(x,0.25)
	m[5] = median(x)
	m[6] = quantile(x,0.75)
	m[7] = max(x)
cat(sprintf(" &%.1f (%.1f) & %.1f & %.1f & %.1f & %.1f & %.1f\\\\ \n",m[1],m[2],m[3],m[4],m[5],m[6],m[7]))
}


 sink(file = "prior_survival_prior.tex")

cat("\\begin{tabular}{r|cccccc} \n")
cat("\\toprule \n")
cat("Characteristic & Mean (sd) & Min & 1st quantile & Median & 3rd quantile & Max \\\\\n")

cat("\\midrule \n")

cat("Median OS (SOC)") 
latex_summary(median_OS_contr)
cat("Median OS (Treated)") 
latex_summary(median_OS_treat)


cat("Median PFS (SOC)") 
latex_summary(median_PFS_contr)
cat("Median PFS (Treated)") 
latex_summary(median_PFS_treat)

cat("Correlation between PFS and OS") 
latex_summary(YS_cor)

cat("Proportion of uncersored PFS") 
latex_summary(PFS_uncens)

cat("Proportion of uncersored OS ") 
latex_summary(OS_uncens)

cat("Time (in days) to collect 50 OS evets" )
latex_summary(t_IA)
cat("\\bottomrule \n")
cat("\\end{tabular}")
sink()

