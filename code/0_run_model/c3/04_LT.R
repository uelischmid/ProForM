### Master Script
### Prepare, run, and analyze LT simulations
### 4.2.23, us



# simsettings -------------------------------------------------------------
source("code/prepsimsettings/ss_c3_LT.R")
rm(list = ls())


# run simulations ---------------------------------------------------------
source("code/0_run_model/1_simulation/simulation_batch_runs_c3_LT.R")
rm(list = ls())


# NaiS assessments --------------------------------------------------------
# prepare
source("code/0_run_model/2_nais_assessment/nais_c3_LT_1_prep.R")
rm(list = ls())

# assess
source("code/0_run_model/2_nais_assessment/nais_c3_LT_2_assess.R")
rm(list = ls())


# analysis ----------------------------------------------------------------
# analysis of ST
source("code/analysis/c3_analysis_LT.R")
rm(list = ls())

# visualization
source("code/visualization/c3/c3_vis_LT.R")

rm(list = ls())

