################
## BATCH RUNS ##
##     OF     ##
## VIS N SIM  ##
################


# setup -------------------------------------------------------------------
vis_groups <- vector(mode = "list", length = 4)

# one list entry for each group of simulations to be visualized together
# within each entry, five vectors have to be defined
# - simulations: simulation names, folders in .../simoutput
# - names: names to distinguish simulations in legend
# - comb_name: name for output folder in ../sim_vis/n_sim/
# - plot_start: first time step to plot (NA for first simulation step)
# - plot_end: last time step to plot (NA for last simulation step)
# - plot_interval: either one number for interval or vector of specific time steps to plot
# - out_subfolder: subfolder for outputs within results/sim_vis/n_sim (either NA or name without /)

vis_groups[[1]]$simulations <- c("SM_example_mgm", "SM_example_nomgm")
vis_groups[[1]]$names <- c("mgm", "no mgm")
vis_groups[[1]]$comb_name <- "SM_example"
vis_groups[[1]]$plot_start <- NA
vis_groups[[1]]$plot_end <- NA
vis_groups[[1]]$plot_interval <- 5
vis_groups[[1]]$out_subfolder <- "examples"

vis_groups[[2]]$simulations <- c("UM_example_mgm", "UM_example_nomgm")
vis_groups[[2]]$names <- c("mgm", "no mgm")
vis_groups[[2]]$comb_name <- "UM_example"
vis_groups[[2]]$plot_start <- NA
vis_groups[[2]]$plot_end <- NA
vis_groups[[2]]$plot_interval <- 5
vis_groups[[2]]$out_subfolder <- "examples"

vis_groups[[3]]$simulations <- c("HM_example_mgm", "HM_example_nomgm")
vis_groups[[3]]$names <- c("mgm", "no mgm")
vis_groups[[3]]$comb_name <- "HM_example"
vis_groups[[3]]$plot_start <- NA
vis_groups[[3]]$plot_end <- NA
vis_groups[[3]]$plot_interval <- 5
vis_groups[[3]]$out_subfolder <- "examples"

vis_groups[[4]]$simulations <- c("SA_example_mgm", "SA_example_nomgm")
vis_groups[[4]]$names <- c("mgm", "no mgm")
vis_groups[[4]]$comb_name <- "SA_example"
vis_groups[[4]]$plot_start <- NA
vis_groups[[4]]$plot_end <- NA
vis_groups[[4]]$plot_interval <- 5
vis_groups[[4]]$out_subfolder <- "examples"


# visualize simulations ---------------------------------------------------
suppressWarnings(suppressMessages(library(tidyverse)))
source("code/visualization/vis_n_sim.R")

time_start <- Sys.time()
for (i in seq_along(vis_groups)) {
  cat(paste0("VIS ", i, "/", length(vis_groups), "\n"))
  PROFORM_vis_n_sim(simulations   = vis_groups[[i]]$simulations,
                    names         = vis_groups[[i]]$names,
                    comb_name     = vis_groups[[i]]$comb_name,
                    plot_start    = vis_groups[[i]]$plot_start,
                    plot_end      = vis_groups[[i]]$plot_end,
                    plot_interval = vis_groups[[i]]$plot_interval,
                    out_subfolder = vis_groups[[i]]$out_subfolder)
  cat("\n")
}

time_end <- Sys.time()
cat("\nBatch run ended.\n",
    length(vis_groups), " simulation-group visualizations in ", round(difftime(time_end, time_start, units = "mins"), 0), " minutes.\n",
    "On average ", round(difftime(time_end, time_start, units = "secs") / length(vis_groups), 0), " seconds per simulation-group visualization.",
    sep = "")
