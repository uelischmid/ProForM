### Master Script
### Prepare, run, and analyze ST simulations
### 27.1.23, us


# aux sim: simsettings ----------------------------------------------------
source("code/prepsimsettings/ss_c3_ST_aux.R")
rm(list = ls())


# aux sim: run simulations ------------------------------------------------
source("code/0_run_model/1_simulation/simulation_batch_runs_c3_ST_aux.R")
rm(list = ls())


# aux sim: NaiS assessments -----------------------------------------------
# prepare
source("code/0_run_model/2_nais_assessment/nais_c3_ST_aux_1_prep.R")
rm(list = ls())

# assess
source("code/0_run_model/2_nais_assessment/nais_c3_ST_aux_2_assess.R")
rm(list = ls())

# visualize
source("code/0_run_model/2_nais_assessment/nais_c3_ST_aux_3_vis.R")
rm(list = ls())

# determine first year of intervention for subsequent simulations
source("code/nais/c3/nais_c3_ST_aux_first_intervention.R")
rm(list = ls())


# ST sim: simsettings -----------------------------------------------------
source("code/prepsimsettings/ss_c3_ST_def.R")
rm(list = ls())


# ST sim: run simulations -------------------------------------------------
source("code/0_run_model/1_simulation/simulation_batch_runs_c3_ST_def.R")
rm(list = ls())


# ST sim: nais analysis ---------------------------------------------------
# prepare
source("code/0_run_model/2_nais_assessment/nais_c3_ST_def_1_prep.R")
rm(list = ls())

# analyze
source("code/0_run_model/2_nais_assessment/nais_c3_ST_def_2_assess.R")
rm(list = ls())


# analysis ----------------------------------------------------------------
# analysis of ST
source("code/analysis/c3_analysis_ST.R")
rm(list = ls())

# visualization
source("code/visualization/c3/c3_vis_ST.R")
rm(list = ls())
