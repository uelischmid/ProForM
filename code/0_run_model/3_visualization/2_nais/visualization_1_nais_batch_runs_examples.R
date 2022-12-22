################
## BATCH RUNS ##
##     OF     ##
## VIS 1 NAIS ##
################


# setup -------------------------------------------------------------------
# nais assessment names: names of folders in .../naisoutput
nais_assessments <- c("SM_example_mgm_LED",
                      "SM_example_nomgm_LED",
                      "UM_example_mgm_TF",
                      "UM_example_nomgm_TF",
                      "HM_example_mgm_RF_rs2sl1",
                      "HM_example_nomgm_RF_rs2sl1",
                      "SA_example_mgm_A",
                      "SA_example_nomgm_A")

# average across n years (NA if all time steps are to be displayed)
avgs_across <- rep(NA, times = length(nais_assessments))

# subfolder for outputs within results/nais_vis/1_sim (either NA or name without /)
subfolder <- rep("examples",
                 times = length(nais_assessments))

# visualize assessments ---------------------------------------------------
suppressWarnings(suppressMessages(library(tidyverse)))
source("code/visualization/vis_1_nais.R")

for (i in seq_along(nais_assessments)) {
  cat(paste0("VIS ", i, "/", length(nais_assessments), ": ", nais_assessments[i], "\n"))
  PROFORM_vis_1_nais(nais_assessment = nais_assessments[i],
                     avg_across      = avgs_across[i],
                     out_subfolder   = subfolder[i])
}

