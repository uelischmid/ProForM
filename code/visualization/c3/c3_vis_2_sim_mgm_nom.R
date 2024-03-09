#####
# visualize model output
# c3: mgm-simulation and corresponding nom-simulation
# 2.1.24, us
#####


PROFORM_vis_2_sim_c3 <- function(simulation_mgm,
                                 plot_start, plot_end, plot_timesteps) {
  # setup -------------------------------------------------------------------
  suppressWarnings(suppressMessages(library(tidyverse)))
  suppressWarnings(suppressMessages(library(cowplot)))
  

  # get NOM-simulation and combination name ---------------------------------
  simulation_nom <- simulation_mgm %>% 
    str_split("_") %>% 
    magrittr::extract2(1)
  
  if (simulation_nom[2] == "ST") {
    simulation_nom[7] <- "NOM"
    simulation_nom[8] <- "t0"
    simulation_nom[9] <- "i0"
  } else if (simulation_nom[2] == "LT") {
    simulation_nom[5] <- "NOM"
    simulation_nom[6] <- "t0"
    simulation_nom[7] <- "i0"
  } else {
    cat("ERROR: only c3 simulations allowed)")
    quit(save = "ask")
  }
  
  simulation_nom <- str_c(simulation_nom, collapse = "_")
  
  # combination name
  comb_name <- simulation_mgm %>% 
    str_replace("c3_", "") %>% 
    str_replace("def_", "")
  
  
  # load data and functions -------------------------------------------------
  # simulation parameters
  param <- read_rds(str_c("data/processed/simoutput/", simulation_mgm, "/3_output/param.rds"))
  
  
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
  res_standdev <- vector(mode = "list", length = 2)
  res_standdev[[1]] <- read_rds(str_c("data/processed/simoutput/", simulation_mgm, "/3_output/res_standdev.rds")) %>% 
    mutate(name = "mgm") %>% 
    filter(time_Step >= ts_start) %>% 
    filter(time_Step <= ts_end) %>% 
    filter(State_name == "fall")
  res_standdev[[2]] <- read_rds(str_c("data/processed/simoutput/", simulation_nom, "/3_output/res_standdev.rds")) %>% 
    mutate(name = "NOM") %>% 
    filter(time_Step >= ts_start) %>% 
    filter(time_Step <= ts_end) %>% 
    filter(State_name == "fall")
  res_standdev <- bind_rows(res_standdev) %>% 
    mutate(name    = factor(name, levels = c("mgm", "NOM")),
           Species = factor(Species, levels = c("none", "aalb", "apse", "fsyl", "pabi")))
  
  # largest cohort per cell
  res_standdev_lc <- res_standdev %>% 
    group_by(name, time_Step2, cc_ID) %>% 
    slice_max(order_by = D, n = 1, with_ties = FALSE) %>% 
    ungroup()
  
  # graph functions
  source("code/visualization/c3/c3_vis_functions.R", local = TRUE)
  
  
  # prepare graphs ----------------------------------------------------------
  # folder for output graphs
  graph_folder <- "results/sim_vis/2_sim_c3/"
  
  # plot basal area ---------------------------------------------------------
  gg_ba_temp <- f.vis_ba_2sim_c3(simstanddev = res_standdev,
                                 dlimit      = 12,
                                 capt        = comb_name)
  
  gg_leg <- get_legend(gg_ba_temp)
  
  gg_ba <- gg_ba_temp +
    theme(legend.position = "none")
  
  
  # plot stem numbers -------------------------------------------------------
  gg_sn <- f.vis_sn_2sim_c3(simstanddev = res_standdev,
                            dlimit      = 12,
                            capt        = comb_name)
  
  
  # plot canopy cover -------------------------------------------------------
  gg_cc <- f.vis_cc_2sim_c3(standdev_lc = res_standdev_lc,
                            capt        = comb_name)
  
  
  # plot crown ratios -------------------------------------------------------
  gg_cr <- f.vis_crdom_2sim_c3(simstanddev = res_standdev,
                               capt        = comb_name)

  
  # plot DBH-distributions --------------------------------------------------
  gg_dd <- f.vis_dd_2sim_c3(simstanddev = res_standdev,
                            dlimit      = 4,
                            ps          = plot_timesteps,
                            capt        = comb_name)
  
  

  # combine plots -----------------------------------------------------------
  gg1 <- plot_grid(gg_ba, gg_sn, gg_cc, gg_cr)
  gg2 <- plot_grid(gg1, gg_dd,
                   nrow  = 2)
  gg3 <- plot_grid(gg2, gg_leg,
                   rel_widths = c(5, 1))
  

  ggsave(filename = str_c(graph_folder, "Sim_", comb_name, ".jpg"),
         plot = gg3,
         width = 17, height = 27, units = "cm",
         scale = 1.5)
   
 }



