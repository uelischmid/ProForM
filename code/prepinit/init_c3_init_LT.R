####
## Extract init LT from simulation runs
## 24.11.22
###


# setup -------------------------------------------------------------------
library(tidyverse)

simulations <- list.dirs("data/processed/simoutput/",
                         full.names = FALSE,
                         recursive  = FALSE)
simulations <- simulations[str_detect(simulations, "c3_su_eq_BG")]

ts_extract <- c(1000, # HM 31
                1000, # HM 33
                1000, # HM 51
                1000, # HM 55
                1000, # SA 31
                1000, # SA 33,
                1000, # SA 51
                1000, # SA 55
                1160, # UM 31
                1075, # UM 33
                1160, # UM 51
                1060) # UM 55

output_location <- rep("c3_init", length(simulations))
dir.create("data/processed/init/c3_init/")

out_names <- c("init_LT_HM_31",
               "init_LT_HM_33",
               "init_LT_HM_51",
               "init_LT_HM_55",
               "init_LT_SA_31",
               "init_LT_SA_33",
               "init_LT_SA_51",
               "init_LT_SA_55",
               "init_LT_UM_31",
               "init_LT_UM_33",
               "init_LT_UM_51",
               "init_LT_UM_55")

source("code/prepinit/functions_init.R")


# create and save inits ---------------------------------------------------
for (i in seq_along(simulations)) {
  f.sim_extract_save2(simname  = simulations[i],
                      ts       = ts_extract[i],
                      out_loc  = output_location[i],
                      out_name = out_names[i])
}


