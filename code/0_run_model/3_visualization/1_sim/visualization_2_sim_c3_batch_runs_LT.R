################
## BATCH RUNS ##
##     OF     ##
## VIS 2 SIM  ##
################

# c3 LT

# setup -------------------------------------------------------------------
library(tidyverse)

# split_vector <- function(x, n = 3) split(x, cut(seq_along(x), n, labels = FALSE))

simulations <- list.dirs("data/processed/simoutput/",
                         full.names = FALSE,
                         recursive  = FALSE)
simulations <- simulations[str_detect(simulations, "c3_LT")]
simulations_mgm <- simulations[str_detect(simulations, "NOM", negate = TRUE)]

p_start <- 901
p_end <- NA
p_timesteps <- c(901, 921, 941, 961, 981, 1000)


# simulations_mgm <- simulations_mgm[str_detect(simulations_mgm, "_UM_", negate = TRUE)]
# simulations_mgm <- split_vector(simulations_mgm)
# simulations_mgm <- simulations_mgm[[3]]

# visualize simulations ---------------------------------------------------
source("code/visualization/c3/c3_vis_2_sim_mgm_nom.R")

time_start <- Sys.time()
for (i in seq_along(simulations_mgm)) {
  cat(paste0("VIS ", i, "/", length(simulations_mgm), "\n"))
  PROFORM_vis_2_sim_c3(simulation_mgm = simulations_mgm[i],
                       plot_start     = p_start,
                       plot_end       = p_end,
                       plot_timesteps = p_timesteps)
  cat("\n")
}

time_end <- Sys.time()
cat("\nBatch run ended.\n",
    length(simulations_mgm), " simulation visualizations in ", round(difftime(time_end, time_start, units = "mins"), 0), " minutes.\n",
    "On average ", round(difftime(time_end, time_start, units = "secs") / length(simulations_mgm), 0), " seconds per simulation visualization.",
    sep = "")
