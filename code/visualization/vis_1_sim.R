#####
# visualize model output
# one simulation
# 7.6.22, us
#####


PROFORM_vis_1_sim <- function(simulation, plot_interval, out_subfolder) {
  # setup -------------------------------------------------------------------
  suppressWarnings(suppressMessages(library(tidyverse)))
  
  # load data and functions -------------------------------------------------
  cat("load data\n")
  
  # simulation parameters
  param <- read_rds(str_c("data/processed/simoutput/", simulation, "/3_output/param.rds"))
  
  # full simulation results
  res_standdev <- read_rds(str_c("data/processed/simoutput/", simulation, "/3_output/res_standdev.rds")) %>% 
    mutate(Species = factor(Species, levels = c("none", "aalb", "apse", "fsyl", "pabi")))
  
  # largest cohort per cell
  res_standdev_lc <- res_standdev %>% 
    group_by(time_Step2, cc_ID) %>% 
    slice_max(order_by = D, n = 1, with_ties = FALSE) %>% 
    ungroup()
  
  # management results & instructions
  if(param$mgm_type != "none") {
    res_mgm <- read_rds(str_c("data/processed/simoutput/", simulation, "/3_output/res_mgm.rds"))
    
    if (param$mgm_type %in% c("RDC_tree", "RDC_cell")) {
      mgm_instructions <- read_rds(str_c("data/processed/simoutput/", simulation, "/3_output/mgm_instructions_RDC.rds"))
    }
  }
  
  # coordinates
  coordinates <- read_rds(str_c("data/processed/simoutput/", simulation, "/3_output/coordinates.rds"))
  
  # graph functions
  source("code/visualization/functions_visualizations.R", local = TRUE)
  
  
  # prepare graphs ----------------------------------------------------------
  # time steps to be plotted
  if (length(plot_interval) == 1) {
    plotsteps <- seq.int(from = 0, to = param$sim_time, by = plot_interval)
    if (tail(plotsteps, 1) < param$sim_time) {
      plotsteps <- c(plotsteps, param$sim_time)
    }
    plotsteps[1] <- 1
  } else {
    plotsteps <- plot_interval
  }
  
  # folder for output graphs
  if (is.na(out_subfolder)) {
    graph_folder <- str_c("results/sim_vis/1_sim/", simulation, "/")
  } else {
    graph_folder <- str_c("results/sim_vis/1_sim/", out_subfolder, "/", simulation, "/")
  }
  
  if (dir.exists(graph_folder)) {
    unlink(str_c(graph_folder, "*"), recursive = TRUE)
  } else {
    dir.create(graph_folder, recursive = TRUE, showWarnings = TRUE)
  }
  
  
  # plot basal area ---------------------------------------------------------
  cat("plot basal area (all trees) \n")
  gg <- f.vis_ba_1sim(standdev = res_standdev,
                      capt     = param$sim_name,
                      dlimit   = 0)
  ggsave(str_c(graph_folder, "01a_ba00_abs.png"),
         width = 5, height = 5, units = "cm",
         scale = 3)
  
  gg <- f.vis_ba_1sim_rel(standdev = res_standdev,
                          capt     = param$sim_name,
                          dlimit   = 0)
  ggsave(str_c(graph_folder, "01a_ba00_rel.png"),
         width = 5, height = 5, units = "cm",
         scale = 3)
  
  cat("plot basal area (>= 12 cm DBH) \n")
  gg <- f.vis_ba_1sim(standdev = res_standdev,
                      capt     = param$sim_name,
                      dlimit   = 12)
  ggsave(str_c(graph_folder, "01b_ba12_abs.png"),
         width = 5, height = 5, units = "cm",
         scale = 3)
  
  gg <- f.vis_ba_1sim_rel(standdev = res_standdev,
                          capt     = param$sim_name,
                          dlimit   = 12)
  ggsave(str_c(graph_folder, "01b_ba12_rel.png"),
         width = 5, height = 5, units = "cm",
         scale = 3)
  
  # plot volume -------------------------------------------------------------
  cat("plot volume (all trees)\n")
  gg <- f.vis_vol_1sim(standdev = res_standdev,
                       capt     = param$sim_name,
                       dlimit   = 0)
  ggsave(str_c(graph_folder, "02a_vol00.png"),
         width = 5, height = 5, units = "cm",
         scale = 3)
  
  cat("plot volume (>= 12 cm DBH)\n")
  gg <- f.vis_vol_1sim(standdev = res_standdev,
                       capt     = param$sim_name,
                       dlimit   = 12)
  ggsave(str_c(graph_folder, "02b_vol12.png"),
         width = 5, height = 5, units = "cm",
         scale = 3)
  
  # plot stem numbers -------------------------------------------------------
  cat("plot stem number (all trees)\n")
  # all trees
  gg <- f.vis_sn_1sim(standdev = res_standdev,
                      capt     = param$sim_name,
                      dlimit   = 0)
  ggsave(str_c(graph_folder, "03a_sn00_abs.png"),
         width = 5, height = 5, units = "cm",
         scale = 3)
  
  gg <- f.vis_sn_1sim_rel(standdev = res_standdev,
                          capt     = param$sim_name,
                          dlimit   = 0)
  ggsave(str_c(graph_folder, "03a_sn00_rel.png"),
         width = 5, height = 5, units = "cm",
         scale = 3)
  
  # stem number (D >= 12)
  cat("plot stem number (>= 12 cm DBH)\n")
  gg <- f.vis_sn_1sim(standdev = res_standdev,
                      capt     = param$sim_name,
                      dlimit   = 12)
  ggsave(str_c(graph_folder, "03b_sn12_abs.png"),
         width = 5, height = 5, units = "cm",
         scale = 3)
  
  gg <- f.vis_sn_1sim_rel(standdev = res_standdev,
                          capt     = param$sim_name,
                          dlimit   = 12)
  ggsave(str_c(graph_folder, "03b_sn12_rel.png"),
         width = 5, height = 5, units = "cm",
         scale = 3)
  
  
  # plot canopy cover -------------------------------------------------------
  cat("plot canopy cover (>= 12 cm DBH)\n")
  gg <- f.vis_cc_1sim(standdev_lc = res_standdev_lc,
                      capt        = param$sim_name)
  ggsave(str_c(graph_folder, "04_cc.png"),
         width = 5, height = 5, units = "cm",
         scale = 3)
  
  
  # plot crown ratios -------------------------------------------------------
  cat("plot crown ratios\n")
  gg <- f.vis_cr_1sim(standdev = res_standdev,
                      capt     = param$sim_name)
  ggsave(str_c(graph_folder, "05_cr.jpg"),
         width = 8, height = 5, units = "cm",
         scale = 3)
  
  
  # plot DBH-distributions --------------------------------------------------
  # all trees
  cat("plot DBH-distribution (all trees)\n")
  gg <- f.vis_dd_1sim(standdev = res_standdev,
                      dlimit   = 0,
                      ps       = plotsteps,
                      capt     = param$sim_name)
  ggsave(str_c(graph_folder, "06a_ddistr00.png"),
         width = 7,
         height = 1.5 * (ceiling(length(plotsteps) / 2) + 1),
         units = "cm",
         scale = 3)
  
  # D >= 12
  cat("plot DBH-distribution (>= 12 cm DBH)\n")
  gg <- f.vis_dd_1sim(standdev = res_standdev,
                      dlimit   = 12,
                      ps       = plotsteps,
                      capt     = param$sim_name)
  ggsave(str_c(graph_folder, "06b_ddistr12.png"),
         width = 7,
         height = 1.5 * (ceiling(length(plotsteps) / 2) + 1),
         units = "cm",
         scale = 3)
  
  
  # plot stage maps ---------------------------------------------------------
  cat("plot stage maps\n")
  gg <- f.vis_stgmps_1sim(standdev_lc = res_standdev_lc,
                          coor        = coordinates,
                          ps          = plotsteps,
                          capt        = param$sim_name)
  ggsave(str_c(graph_folder, "07_stagemaps.png"),
         width = 7,
         height = 1.5 * (ceiling(length(plotsteps) / 2) + 1),
         units = "cm",
         scale = 3)
  
  
  # plot species maps -------------------------------------------------------
  cat("plot species maps\n")
  gg <- f.vis_spcmps_1sim(standdev_lc = res_standdev_lc,
                          coor        = coordinates,
                          ps          = plotsteps,
                          capt        = param$sim_name)
  ggsave(str_c(graph_folder, "08_speciesmaps.png"),
         width = 7,
         height = 1.5 * (ceiling(length(plotsteps) / 2) + 1),
         units = "cm",
         scale = 3)
  
  
  if(param$mgm_type != "none") {
    # plot mgm-DBH-distributions ----------------------------------------------
    cat("plot intervention DBH-distributions\n")
    gg <- f.vis_mgmdd_1sim(mgmres  = res_mgm,
                           dlimit  = 12,
                           capt    = param$sim_name)
    ggsave(str_c(graph_folder, "09a_mgm_ddistr12.jpg"),
           width = 7,
           height = 1.5 * (ceiling(length(param$mgm_interv_steps) / 2) + 1),
           units = "cm",
           scale = 3,
           limitsize = FALSE)
    
    if (param$mgm_type %in% c("RDC_tree", "RDC_cohort")) {
      gg <- f.vis_mgmrdc_1sim(mgmres   = res_mgm,
                              mgminstr = mgm_instructions,
                              capt     = param$sim_name)
      ggsave(str_c(graph_folder, "09b_mgm_ddistrRDC.png"),
             width = 7,
             height = 1.5 * (ceiling(length(param$mgm_interv_steps) / 2) + 1),
             units = "cm",
             scale = 3)
    }
    
    
    # plot intervention maps --------------------------------------------------
    cat("plot intervention maps\n")
    gg <- f.vis_mgmmps_1sim(mgmres  = res_mgm,
                            coor    = coordinates,
                            capt    = param$sim_name)
    ggsave(str_c(graph_folder, "10_mgm_intmaps.png"),
           width = 7,
           height = 1.5 * (ceiling(length(param$mgm_interv_steps) / 2) + 1),
           units = "cm",
           scale = 3,
           limitsize = FALSE)
  }
 
}


