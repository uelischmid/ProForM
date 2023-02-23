### Master Script
### Generate Initialization Data
### 25.1.23, us


# init BG -----------------------------------------------------------------
# saved to data/processed/init/c3_BG
source("code/prepinit/init_c3_BG.R")
rm(list = ls())


# Spin up simulations eq_BG -----------------------------------------------
# simsettings
source("code/prepsimsettings/ss_c3_su_eq_BG.R")
rm(list = ls())

# mgm files
source("code/prepmgm/mgm_c3_su_eq_BG.R")
rm(list = ls())

# run simulations
source("code/0_run_model/1_simulation/simulation_batch_runs_c3_su_eq_BG.R")
rm(list = ls())


# init LT -----------------------------------------------------------------
# saved to data/processed/init/c3_init
source("code/prepinit/init_c3_init_LT.R")
rm(list = ls())


# Spin up simulations eq_mgm ----------------------------------------------
# simsettings
source("code/prepsimsettings/ss_c3_su_eq_mgm.R")
rm(list = ls())

# mgm files
source("code/prepmgm/mgm_c3_su_eq_mgm.R")
rm(list = ls())

# run simulations
source("code/0_run_model/1_simulation/simulation_batch_runs_c3_su_eq_mgm.R")
rm(list = ls())



# NaiS-assessment of eq_mgm to determine time Step for init ST2 -----------
# prepare
source("code/0_run_model/2_nais_assessment/nais_c3_su_eq_mgm_1_prep.R")
rm(list = ls())

# assess
source("code/0_run_model/2_nais_assessment/nais_c3_su_eq_mgm_2_assess.R")
rm(list = ls())

# visualize
source("code/0_run_model/2_nais_assessment/nais_c3_su_eq_mgm_3_vis.R")
rm(list = ls())

# determine years
source("code/nais/c3/nais_c3_su_eq_mgm_time_initST2.R")
rm(list = ls())


# init ST -----------------------------------------------------------------
# saved to data/processed/init/c3_init
source("code/prepinit/init_c3_init_ST.R")
rm(list = ls())


# visualize init files ----------------------------------------------------
source("code/visualization/c3/c3_vis_init.R")
rm(list = ls())
