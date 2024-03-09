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
# run simulations
source("code/0_run_model/1_simulation/simulation_batch_runs_c3_ST_def.R")
rm(list = ls())

# visualize
source("code/0_run_model/3_visualization/1_sim/visualization_2_sim_c3_batch_runs_ST.R")
rm(list = ls())

# ST sim: nais analysis ---------------------------------------------------
# prepare
source("code/0_run_model/2_nais_assessment/nais_c3_ST_def_1_prep.R")
rm(list = ls())

# assess
source("code/0_run_model/2_nais_assessment/nais_c3_ST_def_2_assess_relaxed.R")
rm(list = ls())


# analysis ----------------------------------------------------------------
# analysis of ST
source("code/analysis/c3_analysis_ST_relaxed.R")
rm(list = ls())


# visualize nais ----------------------------------------------------------
# visualize F2
source("code/0_run_model/3_visualization/2_nais/visualization_n_nais_F2_batch_runs_c3_ST.R")
rm(list = ls())

# visualize gaps
source("code/0_run_model/3_visualization/2_nais/visualization_2_nais_gaps_batch_runs_c3_ST.R")
rm(list = ls())

