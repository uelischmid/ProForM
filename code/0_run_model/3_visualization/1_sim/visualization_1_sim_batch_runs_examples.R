################
## BATCH RUNS ##
##     OF     ##
## VIS 1 SIM  ##
################


# setup -------------------------------------------------------------------
# simulations names: names of folders in .../simoutput
simulations <- c("SM_example_mgm",
                 "SM_example_nomgm",
                 "UM_example_mgm",
                 "UM_example_nomgm",
                 "HM_example_mgm",
                 "HM_example_nomgm",
                 "SA_example_mgm",
                 "SA_example_nomgm")

# plot intervals: either one number for interval or vector of specific time steps to plot
plot_intervals <- rep(5,
                      times = length(simulations))

# subfolder for outputs within results/sim_vis/1_sim (either NA or name without /)
subfolder <- rep("examples",
                 times = length(simulations))

# visualize simulations ---------------------------------------------------
suppressWarnings(suppressMessages(library(tidyverse)))
source("code/visualization/vis_1_sim.R")

time_start <- Sys.time()
for (i in seq_along(simulations)) {
  cat(paste0("VIS ", i, "/", length(simulations), ": ", simulations[i], "\n"))
  PROFORM_vis_1_sim(simulation    = simulations[i],
                    plot_interval = plot_intervals[i],
                    out_subfolder = subfolder[i])
  cat("\n")
}

time_end <- Sys.time()
cat("\nBatch run ended.\n",
    length(simulations), " simulation visualizations in ", round(difftime(time_end, time_start, units = "mins"), 0), " minutes.\n",
    "On average ", round(difftime(time_end, time_start, units = "secs") / length(simulations), 0), " seconds per simulation visualization.",
    sep = "")
