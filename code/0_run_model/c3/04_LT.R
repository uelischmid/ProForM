### Master Script
### Prepare, run, and analyze LT simulations
### 4.2.23, us



# simsettings -------------------------------------------------------------
source("code/prepsimsettings/ss_c3_LT.R")
rm(list = ls())


# run simulations ---------------------------------------------------------
# run simulations
source("code/0_run_model/1_simulation/simulation_batch_runs_c3_LT.R")
rm(list = ls())

# visualize
source("code/0_run_model/3_visualization/1_sim/visualization_2_sim_c3_batch_runs_LT.R")
rm(list = ls())

# NaiS assessments --------------------------------------------------------
# prepare
source("code/0_run_model/2_nais_assessment/nais_c3_LT_1_prep.R")
rm(list = ls())

# assess
source("code/0_run_model/2_nais_assessment/nais_c3_LT_2_assess_relaxed.R")
rm(list = ls())


# analysis ----------------------------------------------------------------
# analysis of ST
source("code/analysis/c3_analysis_LT_relaxed.R")
rm(list = ls())


# visualize nais ----------------------------------------------------------
# visualize F2
source("code/0_run_model/3_visualization/2_nais/visualization_n_nais_F2_batch_runs_c3_LT.R")
rm(list = ls())

# visualize subindices (incl. ST)
source("code/0_run_model/3_visualization/2_nais/visualization_2_nais_si_batch_runs_c3_relaxed.R")
rm(list = ls())

# visualize gaps
source("code/0_run_model/3_visualization/2_nais/visualization_2_nais_gaps_batch_runs_c3_LT.R")
rm(list = ls())

