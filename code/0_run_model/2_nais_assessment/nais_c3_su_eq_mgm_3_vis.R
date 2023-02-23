#####################
##   BATCH RUNS    ##
##    OF  NAIS     ##
##  VISUALIZATIONS ##
#####################

# setup -------------------------------------------------------------------
library(tidyverse)

nais_assessments <- list.dirs("data/processed/naisoutput/2_assessment", full.names = FALSE, recursive = FALSE)
nais_assessments <- nais_assessments[str_detect(nais_assessments, "c3_su_eq_mgm")]

# visualization of subindices
out_si <- rep("c3_su_eq_mgm", length(nais_assessments)) # subfolder for outputs within results/nais_vis/1_sim_subind/, NA if none


# run visualization subindices --------------------------------------------
source("code/visualization/vis_1_nais_subindices.R")
for (i in seq_along(nais_assessments)) {
  cat(str_c("VIS SUBINDICES ", i, "/", length(nais_assessments), ": ", nais_assessments[i], "\n"))
  PROFORM_vis_1_nais_si(nais_assessment = nais_assessments[i],
                        out_subfolder   = out_si[i])
}
