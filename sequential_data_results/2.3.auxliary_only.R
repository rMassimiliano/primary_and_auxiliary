suppressMessages({
  library(dplyr);
  library(readr);
  library(gsDesign)
})

data_path = 'data/'
scenarios_names = c("H0_omega_1",
                    "H0_omega_2",
                    "H0_omega_10",
                    "H01_omega_1",
                    "H01_omega_2",
                    "H01_omega_10",
                    "H10_omega_1",
                    "H10_omega_2",
                    "H10_omega_10",
                    "H11_omega_1",
                    "H11_omega_2",
                    "H11_omega_10",
                    "Hmiss_omega_1",
                    "Hmiss_omega_2",
                    "Hmiss_omega_10")


data_list = list.files("data/H01_omega_10")
N = 200
N_IA =N/2
IA_futility_threshold  = 0.8

source("../R/compute_Zs.R")

Te_to_Zy = \(x) {res = x["Delta_Y"]/sqrt(x["Var_Delta_Y"]); names(res) =NULL;res }
Te_to_Zs = \(x) {res = x["Delta_S"]/sqrt(x["Var_Delta_S"]); names(res) =NULL;res }

## using default 
ALPHA = 0.05
GAMMA = 2
mod = gsDesign(k = 2, test.type = 1, n.fix = N, alpha = ALPHA , beta =0.2,endpoint = 'Binomial', sfupar = GAMMA)
C1     = mod$upper$bound[1]
C2     = mod$upper$bound[2]


surrogate_analysis = list()
for(current_scenario in scenarios_names)
{
 data_list = list.files(sprintf('%s%s', data_path,current_scenario))
 decision = character(length(data_list))
 for(r in 1:length(data_list))
 {
  dat = read.csv(sprintf('%s%s/%s',data_path,current_scenario,data_list[r]))
    Z_Y_IA  =  Te_to_Zs(compute_Zs(dat[1:N_IA,]))
    Z_Y_FA  =  Te_to_Zs(compute_Zs(dat))
    C_power = condPower(Z_Y_IA,N, z2 = z2NC, alpha = ALPHA, x = mod, timing = 0.5)
    if(C_power<= IA_futility_threshold)  dec  = 'futility-stop'
    else if(Z_Y_IA>=C1) dec  = 'efficacy-stop'
    else if(Z_Y_FA>= C2) dec = 'H1'
    else dec ='H0'
    decision[r] = dec
  if(r %%100==0) {print(sprintf("done with file %i",r))}
 } 
 surrogate_analysis[[current_scenario]] = decision
}

save(surrogate_analysis, file = 'surrogate_analysis.rdata')



