################
## BATCH RUNS ##
##     OF     ##
## VIS 1 NAIS ##
##     F2     ##
################


# setup -------------------------------------------------------------------
library(tidyverse)

# nais assessment names: names of folders in .../naisoutput/2_assessment/
nais_assessments <- c("c3_LT_HM_55_NOM_t0_i0_A",
                      "c3_LT_HM_55_NOM_t0_i0_LED")

# average across n years (NA if all time steps are to be displayed)
avgs <- rep(NA, length(nais_assessments))

# subfolder for outputs within results/nais_vis/1_sim_F2/ (either NA or name without /)
out_F2 <- rep("examples", length(nais_assessments))


# run visualization F2 ----------------------------------------------------
source("code/visualization/vis_1_nais_F2.R")
for (i in seq_along(nais_assessments)) {
  cat(str_c("VIS F2 ", i, "/", length(nais_assessments), ": ", nais_assessments[i], "\n"))
  PROFORM_vis_1_nais_F2(nais_assessment = nais_assessments[i],
                        avg_across      = avgs[i],
                        out_subfolder   = out_F2[i])
}
