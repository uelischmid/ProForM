################
## BATCH RUNS ##
##     OF     ##
## VIS 1 NAIS ##
## SUBINDICES ##
################

## c3 relaxed
## mgm and corresponding NOM simulation

# setup -------------------------------------------------------------------
library(tidyverse)

# split_vector <- function(x, n = 3) split(x, cut(seq_along(x), n, labels = FALSE))

# nais assessment names: names of folders in .../naisoutput/2_assessment/
nais_assessments <- list.dirs("data/processed/naisoutput/2_assessment_relaxed/",
                         full.names = FALSE,
                         recursive  = FALSE)
nais_assessments <- nais_assessments[str_detect(nais_assessments, "c3_LT|c3_ST_def")]
nais_assessments_mgm <- nais_assessments[str_detect(nais_assessments, "NOM", negate = TRUE)]


# subfolder for outputs within results/nais_vis/1_sim_subind/, NA if none
out_si <- "c3_relaxed_mgm_nom" # subfolder for outputs within results/nais_vis/1_sim_subind/, NA if none


# nais_assessments_mgm <- split_vector(nais_assessments_mgm)
# nais_assessments_mgm <- nais_assessments_mgm[[3]]


# run visualization F2 ----------------------------------------------------
source("code/visualization/c3/c3_vis_2_nais_subindices_relaxed_mgm_nom.R")
for (i in seq_along(nais_assessments_mgm)) {
  cat(str_c("VIS SUBINDICES ", i, "/", length(nais_assessments_mgm), ": ", nais_assessments_mgm[i], "\n"))
  PROFORM_vis_2_nais_si_relaxed_c3(nais_assessment_mgm = nais_assessments_mgm[i],
                                   out_subfolder       = out_si)
}
