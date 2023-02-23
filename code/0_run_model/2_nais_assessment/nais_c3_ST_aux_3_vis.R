#####################
##   BATCH RUNS    ##
##    OF  NAIS     ##
##  VISUALIZATIONS ##
#####################

# setup -------------------------------------------------------------------
library(tidyverse)

nais_assessments <- list.dirs("data/processed/naisoutput/2_assessment", full.names = FALSE, recursive = FALSE)
nais_assessments <- nais_assessments[str_detect(nais_assessments, "c3_ST_aux")]

# visualization of F2
avgs <- rep(NA, length(nais_assessments)) # NA for no averaging or number of time_Steps else
out_F2 <- rep("c3_ST_aux", length(nais_assessments)) # subfolder for outputs within results/nais_vis/1_sim_F2/, NA if none

# visualization of subindices
out_si <- rep("c3_ST_aux", length(nais_assessments)) # subfolder for outputs within results/nais_vis/1_sim_subind/, NA if none

# run visualization F2 ----------------------------------------------------
source("code/visualization/vis_1_nais_F2.R")
for (i in seq_along(nais_assessments)) {
  cat(str_c("VIS F2 ", i, "/", length(nais_assessments), ": ", nais_assessments[i], "\n"))
  PROFORM_vis_1_nais_F2(nais_assessment = nais_assessments[i],
                        avg_across      = avgs[i],
                        out_subfolder   = out_F2[i])
}


# run visualization subindices --------------------------------------------
source("code/visualization/vis_1_nais_subindices.R")
for (i in seq_along(nais_assessments)) {
  cat(str_c("VIS SUBINDICES ", i, "/", length(nais_assessments), ": ", nais_assessments[i], "\n"))
  PROFORM_vis_1_nais_si(nais_assessment = nais_assessments[i],
                        out_subfolder   = out_si[i])
}

