################
## BATCH RUNS ##
##     OF     ##
## VIS 2 GAPS ##
################

## c3 LT

# setup -------------------------------------------------------------------
library(tidyverse)

# split_vector <- function(x, n = 3) split(x, cut(seq_along(x), n, labels = FALSE))

simulations <- list.dirs("data/processed/naisoutput/1_prep/",
                         full.names = FALSE,
                         recursive  = FALSE)
simulations <- simulations[str_detect(simulations, "c3_LT")]
simulations_mgm <- simulations[str_detect(simulations, "NOM", negate = TRUE)]

p_timesteps <- c(901, 921, 941, 961, 981, 1000)

o_subfolder <- "c3_mgm_NOM"


# simulations_mgm <- split_vector(simulations_mgm)
# simulations_mgm <- simulations_mgm[[1]]


# run gap visualizations --------------------------------------------------
source("code/visualization/c3/c3_vis_2_gaps_mgm_nom.R")

for (i in seq_along(simulations_mgm)) {
  cat(str_c("VIS gaps ", i, "/", length(simulations_mgm), "\n"))
  PROFORM_vis_2_gaps_c3(simulation_mgm = simulations_mgm[i],
                        plot_timesteps = p_timesteps,
                        out_subfolder  = o_subfolder) 
}
