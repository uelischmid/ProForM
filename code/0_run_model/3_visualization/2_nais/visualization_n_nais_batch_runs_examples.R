################
## BATCH RUNS ##
##     OF     ##
## VIS N NAIS ##
################


# setup -------------------------------------------------------------------
vis_groups <- vector(mode = "list", length = 4)

# one list entry for each group of assessments to be visualized together
# within each entry, four vectors have to be defined
# - nais_assessments: names of folders in .../sim_nais
# - names: names to distinguish assessments in legend
# - comb_name: name for output file in ../nais_vis/n_sim/
# - avg_across: average results across n years. NA if every year should be displayed
# - out_subfolder: subfolder for outputs within results/nais_vis/n_sim (either NA or name without /)

vis_groups[[1]]$nais_assessments <- c("SM_example_mgm_LED", "SM_example_nomgm_LED")
vis_groups[[1]]$names <- c("mgm", "no_mgm")
vis_groups[[1]]$comb_name <- "SM_example"
vis_groups[[1]]$avg_across <- NA
vis_groups[[1]]$out_subfolder <- "examples"

vis_groups[[2]]$nais_assessments <- c("UM_example_mgm_TF", "UM_example_nomgm_TF")
vis_groups[[2]]$names <- c("mgm", "no_mgm")
vis_groups[[2]]$comb_name <- "UM_example"
vis_groups[[2]]$avg_across <- NA
vis_groups[[2]]$out_subfolder <- "examples"

vis_groups[[3]]$nais_assessments <- c("HM_example_mgm_RF_rs2sl1", "HM_example_nomgm_RF_rs2sl1")
vis_groups[[3]]$names <- c("mgm", "no_mgm")
vis_groups[[3]]$comb_name <- "HM_example"
vis_groups[[3]]$avg_across <- NA
vis_groups[[3]]$out_subfolder <- "examples"

vis_groups[[4]]$nais_assessments <- c("SA_example_mgm_A", "SA_example_nomgm_A")
vis_groups[[4]]$names <- c("mgm", "no_mgm")
vis_groups[[4]]$comb_name <- "SA_example"
vis_groups[[4]]$avg_across <- NA
vis_groups[[4]]$out_subfolder <- "examples"

# visualize assessments ---------------------------------------------------
suppressWarnings(suppressMessages(library(tidyverse)))
source("code/visualization/vis_n_nais.R")

for (i in seq_along(vis_groups)) {
  cat(paste0("VIS ", i, "/", length(vis_groups), "\n"))
  PROFORM_vis_n_nais(nais_assessments = vis_groups[[i]]$nais_assessments,
                     names            = vis_groups[[i]]$names,
                     comb_name        = vis_groups[[i]]$comb_name,
                     avg_across       = vis_groups[[i]]$avg_across,
                     out_subfolder    = vis_groups[[i]]$out_subfolder)
}

