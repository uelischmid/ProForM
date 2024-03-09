################
## BATCH RUNS ##
##     OF     ##
## VIS N NAIS ##
##     F2     ##
################

## c3 LT relaxed, NOM & MGM

# setup -------------------------------------------------------------------
library(tidyverse)

assessments <- list.dirs("data/processed/naisoutput/2_assessment_relaxed/",
                         full.names = FALSE,
                         recursive  = FALSE)
assessments <- assessments[str_detect(assessments, "c3_LT_")]
assessments_mgm <- assessments[str_detect(assessments, "NOM", negate = TRUE)]

analysis_relaxed <- read_rds("results/c3_analysis_LT/LT_relaxed.rds") %>% 
  mutate(temp_sim_ID = str_c(stratum, "_", quality, "_",
                             mgm, "_t", mgm_interval, "_i", mgm_intensity, "_",
                             nat_haz))

vis_groups <- vector(mode = "list", length = length(assessments_mgm))

# one list entry for each group of assessments to be visualized together
# within each entry, four vectors have to be defined
# - nais_assessments: names of folders in data/processed/naisoutput/2_assessment/
# - names: names to distinguish assessments in legend
# - capt: caption (either text or NULL)
# - comb_name: name for subtitle in graph and output file in results/nais_vis/n_sim_F2/
# - avg_across: average results across n years. NA if every year should be displayed
# - out_subfolder: subfolder for outputs within results/nais_vis/n_sim_F2 (either NA or name without /)

for (i in seq_along(vis_groups)) {
  ass_mgm <- assessments_mgm[i] 
  ass_NOM <- ass_mgm %>% 
    str_split("_") %>% 
    magrittr::extract2(1)
  ass_NOM[5] <- "NOM"
  ass_NOM[6] <- "t0"
  ass_NOM[7] <- "i0"
  ass_NOM <- str_c(ass_NOM, collapse = "_")
  
  vis_groups[[i]]$nais_assessments <- c(ass_NOM, ass_mgm)
  
  vis_groups[[i]]$names <- c("NOM", "mgm")
  
  analysis_values_NOM <- filter(analysis_relaxed,
                                temp_sim_ID == str_replace(ass_NOM, "c3_LT_", ""))
  analysis_values_mgm <- filter(analysis_relaxed,
                                temp_sim_ID == str_replace(ass_mgm, "c3_LT_", ""))
  
  vis_groups[[i]]$capt <- str_c(
    "NOM: sMPabs = ", round(pull(analysis_values_NOM, sha_i_MP_met), 2),
    "; sIPabs = ", round(pull(analysis_values_NOM, sha_i_IP_met), 2),
    "\nMGM: sMPabs = ", round(pull(analysis_values_mgm, sha_i_MP_met), 2),
    "; sIPabs = ", round(pull(analysis_values_mgm, sha_i_IP_met), 2)
  )
  
  vis_groups[[i]]$comb_name <- ass_mgm %>% 
    str_replace("c3_", "")
  
  vis_groups[[i]]$avg_across <- NA
  
  vis_groups[[i]]$out_subfolder <- "c3_mgm_NOM"
}


# run visualization F2 ----------------------------------------------------
source("code/visualization/vis_n_nais_F2_relaxed.R")
  
for (i in seq_along(vis_groups)) {
  cat(str_c("VIS F2 ", i, "/", length(vis_groups), "\n"))
  PROFORM_vis_n_nais_F2_relaxed(nais_assessments = vis_groups[[i]]$nais_assessments,
                                names            = vis_groups[[i]]$names,
                                capt             = vis_groups[[i]]$capt,
                                comb_name        = vis_groups[[i]]$comb_name,
                                avg_across       = vis_groups[[i]]$avg_across,
                                out_subfolder    = vis_groups[[i]]$out_subfolder)
}
