####
## Extract init-data from simulation run
## 24.11.22
###


# setup -------------------------------------------------------------------
library(tidyverse)

simulations <- list.dirs("data/processed/simoutput/",
                         full.names = FALSE,
                         recursive  = FALSE)
simulations_eq_BG <- simulations[str_detect(simulations, "c3_su_eq_BG")]
simulations_eq_mgm <- simulations[str_detect(simulations, "c3_su_eq_mgm")]

simulations <- vector(mode = "character")

for (i in seq_along(simulations_eq_BG)) {
  simulations <- c(simulations, c(simulations_eq_BG[i],
                                  simulations_eq_mgm[i],
                                  simulations_eq_BG[i]))
}


ts_extract <- c( 65, 920, 160, # HM 31: init ST1, init ST2, init ST3
                 50, 960, 160, # HM 33
                 65, 978, 160, # HM 51
                 35, 901, 160, # HM 55
                 90, 901, 185, # SA 31
                 60, 914, 185, # SA 33
                 90, 901, 185, # SA 51
                 40, 901, 185, # SA 55
                205, 905, 330, # UM 31
                120, 959, 270, # UM 33
                200, 943, 325, # UM 51
                100, 921, 185) # UM 55

output_location <- rep("c3_init", length(simulations))

out_names <- c("init_ST1_HM_31", "init_ST2_HM_31", "init_ST3_HM_31",
               "init_ST1_HM_33", "init_ST2_HM_33", "init_ST3_HM_33",
               "init_ST1_HM_51", "init_ST2_HM_51", "init_ST3_HM_51",
               "init_ST1_HM_55", "init_ST2_HM_55", "init_ST3_HM_55",
               "init_ST1_SA_31", "init_ST2_SA_31", "init_ST3_SA_31",
               "init_ST1_SA_33", "init_ST2_SA_33", "init_ST3_SA_33",
               "init_ST1_SA_51", "init_ST2_SA_51", "init_ST3_SA_51",
               "init_ST1_SA_55", "init_ST2_SA_55", "init_ST3_SA_55",
               "init_ST1_UM_31", "init_ST2_UM_31", "init_ST3_UM_31",
               "init_ST1_UM_33", "init_ST2_UM_33", "init_ST3_UM_33",
               "init_ST1_UM_51", "init_ST2_UM_51", "init_ST3_UM_51",
               "init_ST1_UM_55", "init_ST2_UM_55", "init_ST3_UM_55")

source("code/prepinit/functions_init.R")


# create and save inits ---------------------------------------------------
for (i in seq_along(simulations)) {
  f.sim_extract_save2(simname  = simulations[i],
                      ts       = ts_extract[i],
                      out_loc  = output_location[i],
                      out_name = out_names[i])
}


