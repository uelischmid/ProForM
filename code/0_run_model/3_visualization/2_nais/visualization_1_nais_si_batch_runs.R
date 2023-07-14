################
## BATCH RUNS ##
##     OF     ##
## VIS 1 NAIS ##
## SUBINDICES ##
################


# setup -------------------------------------------------------------------
library(tidyverse)

# nais assessment names: names of folders in .../naisoutput/2_assessment/
nais_assessments <- c("c3_LT_HM_55_NOM_t0_i0_A",
                      "c3_LT_HM_55_NOM_t0_i0_LED")

# subfolder for outputs within results/nais_vis/1_sim_subind/, NA if none
out_si <- rep("examples", length(nais_assessments))


# run visualization F2 ----------------------------------------------------
source("code/visualization/vis_1_nais_subindices.R")
for (i in seq_along(nais_assessments)) {
  cat(str_c("VIS SUBINDICES ", i, "/", length(nais_assessments), ": ", nais_assessments[i], "\n"))
  PROFORM_vis_1_nais_si(nais_assessment = nais_assessments[i],
                        out_subfolder   = out_si[i])
}
