#####
# visualize model output
# multiple simulations
# 7.6.22, us
#####


PROFORM_vis_n_sim <- function(simulations, names, comb_name,
                               plot_start, plot_end, plot_interval,
                               out_subfolder) {
  # setup -------------------------------------------------------------------
  suppressWarnings(suppressMessages(library(tidyverse)))
  
  # load data and functions -------------------------------------------------
  cat("load data\n")
  
  # simulation parameters
  param <- vector(mode = "list", length = length(simulations))
  for (i in seq_along(param)) {
    param[[i]] <- read_rds(str_c("data/processed/simoutput/", simulations[i], "/3_output/param.rds"))
  }
  
  if (sum(map(param, "sim_area_ha") %in% param[[1]]$sim_area_ha) != length(param)) {
    cat("ERROR: simulation areas do not match.\n(hit escape to stop execution)")
    quit(save = "ask")
  }
  
  if (sum(map(param, "sim_ncells") %in% param[[1]]$sim_ncells) != length(param)) {
    cat("ERROR: number of cells do not match.\n(hit escape to stop execution)")
    quit(save = "ask")
  }
  
  param <- param[[1]]
  
  # restrict time steps
  if (is.na(plot_start)) {
    ts_start <- 0
  } else {
    ts_start <- plot_start
  }
  
  if (is.na(plot_end)) {
    ts_end <- param$sim_time
  } else {
    ts_end <- plot_end
  }
  
  # full simulation results
  res_standdev <- vector(mode = "list", length = length(simulations))
  for (i in seq_along(res_standdev)) {
    res_standdev[[i]] <- read_rds(str_c("data/processed/simoutput/", simulations[i], "/3_output/res_standdev.rds")) %>% 
      mutate(name = names[i]) %>% 
      filter(time_Step >= ts_start) %>% 
      filter(time_Step <= ts_end)
  }
  res_standdev <- bind_rows(res_standdev) %>% 
    mutate(name    = factor(name, levels = names),
           Species = factor(Species, levels = c("none", "aalb", "apse", "fsyl", "pabi")))
  
  # largest cohort per cell
  res_standdev_lc <- res_standdev %>% 
    group_by(name, time_Step2, cc_ID) %>% 
    slice_max(order_by = D, n = 1, with_ties = FALSE) %>% 
    ungroup()
  
  # graph functions
  source("code/visualization/functions_visualizations.R", local = TRUE)
  
  
  # prepare graphs ----------------------------------------------------------
  # time steps to be plotted
  if (length(plot_interval) == 1) { # generic interval
    plotsteps <- seq.int(from = ts_start, to = ts_end, by = plot_interval)
    if (tail(plotsteps, 1) < ts_end) {
      plotsteps <- c(plotsteps, ts_end)
    }
    if(head(plotsteps, 1) == 0) {
      plotsteps[1] <- 1
    }
  } else { # specific time steps
    if (sum(plot_interval %in% c(ts_start:ts:end)) != length(plot_interval)) {
      cat("ERROR: selected time steps to plot not within\ninterval of plot_start and plot_end.\n(hit escape to stop execution)")
      quit(save = "ask")
    } else {
      plotsteps <- plot_interval
    }
  }
  
  # folder for output graphs
  if (is.na(out_subfolder)) {
    graph_folder <- str_c("results/sim_vis/n_sim/", comb_name, "/")
  } else {
    graph_folder <- str_c("results/sim_vis/n_sim/", out_subfolder, "/", comb_name, "/")
  }
  
  if (dir.exists(graph_folder)) {
    unlink(str_c(graph_folder, "*"), recursive = TRUE)
  } else {
    dir.create(graph_folder, recursive = TRUE, showWarnings = TRUE)
  }
  
  # plot basal area ---------------------------------------------------------
  # all trees
  cat("plot basal area (all trees)\n")
  gg <- f.vis_ba_nsim_efm(simstanddev = res_standdev,
                          efm         = NULL,
                          targetrange = NA,
                          dlimit      = 0,
                          capt        = comb_name)
  ggsave(str_c(graph_folder, "comb_01a_ba00.png"),
         width = 5, height = 5, units = "cm",
         scale = 3)
  
  for (i in seq_along(names)) {
    temp_res_standdev <- res_standdev %>%
      filter(name == names[i])
    
    gg <- f.vis_ba_1sim(standdev = temp_res_standdev,
                        capt     = str_c(names[i], " / ", simulations[i]),
                        dlimit   = 0)
    ggsave(str_c(graph_folder, "spc_01a_ba00_", names[i], ".png"),
           width = 5, height = 5, units = "cm",
           scale = 3)
  }
  
  # D >= 12
  cat("plot basal area (>= 12 cm DBH)\n")
  gg <- f.vis_ba_nsim_efm(simstanddev = res_standdev,
                          efm         = NULL,
                          targetrange = NA,
                          dlimit      = 12,
                          capt        = comb_name)
  ggsave(str_c(graph_folder, "comb_01b_ba12.png"),
         width = 5, height = 5, units = "cm",
         scale = 3)
  
  for (i in seq_along(names)) {
    temp_res_standdev <- res_standdev %>%
      filter(name == names[i])
    
    gg <- f.vis_ba_1sim(standdev = temp_res_standdev,
                        capt     = str_c(names[i], " / ", simulations[i]),
                        dlimit   = 12)
    ggsave(str_c(graph_folder, "spc_01b_ba12_", names[i], ".png"),
           width = 5, height = 5, units = "cm",
           scale = 3)
  }
  
  # plot volume -------------------------------------------------------------
  # all trees
  cat("plot volume (all trees)\n")
  gg <- f.vis_vol_nsim(standdev = res_standdev,
                       capt     = comb_name,
                       dlimit   = 0)
  ggsave(str_c(graph_folder, "comb_02a_vol00.png"),
         width = 5, height = 5, units = "cm",
         scale = 3)
  
  for (i in seq_along(names)) {
    temp_res_standdev <- res_standdev %>%
      filter(name == names[i])
    
    gg <- f.vis_vol_1sim(standdev = temp_res_standdev,
                         capt     = str_c(names[i], " / ", simulations[i]),
                         dlimit   = 0)
    ggsave(str_c(graph_folder, "spc_02a_vol00_", names[i], ".png"),
           width = 5, height = 5, units = "cm",
           scale = 3)
  }
  
  # D >= 12
  cat("plot volume (>= 12 cm DBH)\n")
  gg <- f.vis_vol_nsim(standdev = res_standdev,
                       capt     = comb_name,
                       dlimit   = 12)
  ggsave(str_c(graph_folder, "comb_02b_vol12.png"),
         width = 5, height = 5, units = "cm",
         scale = 3)
  
  for (i in seq_along(names)) {
    temp_res_standdev <- res_standdev %>%
      filter(name == names[i])
    
    gg <- f.vis_vol_1sim(standdev = temp_res_standdev,
                         capt     = str_c(names[i], " / ", simulations[i]),
                         dlimit   = 12)
    ggsave(str_c(graph_folder, "spc_02b_vol12_", names[i], ".png"),
           width = 5, height = 5, units = "cm",
           scale = 3)
  }
  
  # plot stem numbers -------------------------------------------------------
  # all trees
  cat("plot stem number (all trees)\n")
  gg <- f.vis_sn_nsim_efm(simstanddev = res_standdev,
                          efm         = NULL,
                          targetrange = NA,
                          dlimit      = 0,
                          capt        = comb_name)
  ggsave(str_c(graph_folder, "comb_03a_sn00.png"),
         width = 5, height = 5, units = "cm",
         scale = 3)
  
  for (i in seq_along(names)) {
    temp_res_standdev <- res_standdev %>%
      filter(name == names[i])
    
    gg <- f.vis_sn_1sim(standdev = temp_res_standdev,
                        capt     = str_c(names[i], " / ", simulations[i]),
                        dlimit   = 0)
    ggsave(str_c(graph_folder, "spc_03a_sn00_", names[i], ".png"),
           width = 5, height = 5, units = "cm",
           scale = 3)
  }
  
  
  # D >= 12
  cat("plot stem number (>= 12 cm DBH)\n")
  gg <- f.vis_sn_nsim_efm(simstanddev = res_standdev,
                          efm         = NULL,
                          targetrange = NA,
                          dlimit      = 12,
                          capt        = comb_name)
  ggsave(str_c(graph_folder, "comb_03b_sn12.png"),
         width = 5, height = 5, units = "cm",
         scale = 3)
  
  for (i in seq_along(names)) {
    temp_res_standdev <- res_standdev %>%
      filter(name == names[i])
    
    gg <- f.vis_sn_1sim(standdev = temp_res_standdev,
                        capt     = str_c(names[i], " / ", simulations[i]),
                        dlimit   = 12)
    ggsave(str_c(graph_folder, "spc_03b_sn12_", names[i], ".png"),
           width = 5, height = 5, units = "cm",
           scale = 3)
  }
  
  
  # plot canopy cover -------------------------------------------------------
  cat("plot canopy cover (>= 12 cm DBH)\n")
  gg <- f.vis_cc_nsim(standdev_lc = res_standdev_lc,
                      capt        = comb_name)
  ggsave(str_c(graph_folder, "comb_04_cc.png"),
         width = 5, height = 5, units = "cm",
         scale = 3)
  
  
  # plot crown ratios -------------------------------------------------------
  cat("plot crown ratios\n")
  gg <- f.vis_cr_nsim_efm(simstanddev = res_standdev,
                          efm         = NULL,
                          targetrange = NA,
                          myears      = NA,
                          capt        = comb_name)
  ggsave(str_c(graph_folder, "comb_05a_cr.png"),
         width = 8, height = 10, units = "cm",
         scale = 3)
  
  gg <- f.vis_crdom_nsim_efm(simstanddev = res_standdev,
                             efm         = NULL,
                             targetrange = NA,
                             myears      = NA,
                             capt        = comb_name)
  ggsave(str_c(graph_folder, "comb_05b_cr_dom100.png"),
         width = 4, height = 10, units = "cm",
         scale = 3)
  
  # plot DBH-distributions --------------------------------------------------
  # all trees
  cat("plot DBH-distribution (all trees)\n")
  gg <- f.vis_dd_nsim_efm(simstanddev = res_standdev,
                          efm         = NULL,
                          targetrange = NA,
                          myears      = NA,
                          dlimit      = 0,
                          ps          = plotsteps,
                          capt        = comb_name)
  
  ggsave(str_c(graph_folder, "comb_06a_dd00.png"),
         width = 7,
         height = 1.5 * (ceiling(length(plotsteps) / 2) + 1),
         units = "cm",
         scale = 3)
  
  for (i in seq_along(names)) {
    temp_res_standdev <- res_standdev %>%
      filter(name == names[i])
    
    gg <- f.vis_dd_1sim(standdev = temp_res_standdev,
                        dlimit   = 0,
                        ps       = plotsteps,
                        capt     = str_c(names[i], " / ", simulations[i]))
    ggsave(str_c(graph_folder, "spc_06a_ddistr00_", names [i], ".png"),
           width = 7,
           height = 1.5 * (ceiling(length(plotsteps) / 2) + 1),
           units = "cm",
           scale = 3)
    
  }
  
  
  # D >= 12
  cat("plot DBH-distribution (>= 12 cm DBH)\n")
  gg <- f.vis_dd_nsim_efm(simstanddev = res_standdev,
                          efm         = NULL,
                          targetrange = NA,
                          myears      = NA,
                          dlimit      = 12,
                          ps          = plotsteps,
                          capt        = comb_name)
  ggsave(str_c(graph_folder, "comb_06b_dd12.png"),
         width = 7,
         height = 1.5 * (ceiling(length(plotsteps) / 2) + 1),
         units = "cm",
         scale = 3)
  
  for (i in seq_along(names)) {
    temp_res_standdev <- res_standdev %>%
      filter(name == names[i])
    
    gg <- f.vis_dd_1sim(standdev = temp_res_standdev,
                        dlimit   = 12,
                        ps       = plotsteps,
                        capt     = str_c(names[i], " / ", simulations[i]))
    ggsave(str_c(graph_folder, "spc_06b_ddistr12_", names[i], ".png"),
           width = 7,
           height = 1.5 * (ceiling(length(plotsteps) / 2) + 1),
           units = "cm",
           scale = 3)
  }
  
  
  cat("---FINISHED---\n")
}



