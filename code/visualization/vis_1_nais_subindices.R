#####
# visualize nais assessment
# all subindices
# one simulation
# 7.12.22, us
#####

PROFORM_vis_1_nais_si <- function(nais_assessment, out_subfolder) {
  
  # setup -------------------------------------------------------------------
  suppressWarnings(suppressMessages(library(tidyverse)))
  suppressWarnings(suppressMessages(library(cowplot)))
  
  
  # load assessment, profile & plot functions -------------------------------
  folder <- str_c("data/processed/naisoutput/2_assessment/", nais_assessment, "/")
  ind_full <- read_rds(str_c(folder, "nais_indices_full.rds"))
  ind_red <- read_rds(str_c(folder, "nais_indices_red.rds"))
  stand <- read_rds(str_c(folder, "nais_stand.rds"))
  
  if (file.exists(str_c(folder, "nais_profile.rds")) == TRUE) {
    nais_profile <- read_rds(str_c(folder, "nais_profile.rds"))
    natural_hazard <- unique(ind_full$natural_hazard)
  } else {
    info <- read.table(str_c(folder, "info.txt"), header = TRUE, nrows = 6, na.strings = "<NA>") %>% 
      as_tibble()
    param <- read_rds(str_c("data/processed/simoutput/",
                            info[[1, 2]], "/3_output/param.rds"))
    stratum_fine <- info[[3, 2]]
    natural_hazard <- info[[4, 2]]
    rockfall_scenario <- info[[5, 2]]
    source("code/nais/nais_profiles.R", local = TRUE)
  }
  
  source("code/visualization/functions_visualizations.R", local = TRUE)
  
  
  # create or empty folder --------------------------------------------------
  out_folder <- "results/nais_vis/1_sim_subind/"
  if (!is.na(out_subfolder)){
    out_folder <- str_c(out_folder, out_subfolder, "/")
  }
  
  out_folder <- str_c(out_folder, nais_assessment, "/")
  if (dir.exists(out_folder)) {
    unlink(str_c(out_folder, "*"), recursive = TRUE)
  } else {
    dir.create(out_folder, recursive = TRUE, showWarnings = TRUE)
  }
  

  # plot F2 -----------------------------------------------------------------
  p <- f.vis_nais_F2(i_r = ind_red,
                     ass = nais_assessment,
                     tc  = theme_custom)
  suppressWarnings(ggsave(filename = str_c(out_folder, "0_F2.png"),
                          width = 5,
                          height = 15,
                          units = "cm",
                          scale = 2))
  
  
  # plot mix ----------------------------------------------------------------
  p <- f.vis_nais_si_mix_stand(st = stand,
                               np = nais_profile,
                               tc = theme_custom)
  suppressMessages(ggsave(filename = str_c(out_folder, "1a_mix_stand.png"),
                          plot     = p))
  
  p <- f.vis_nais_si_mix_ind(i_f = ind_full,
                             tc  = theme_custom)
  suppressMessages(ggsave(filename = str_c(out_folder, "1b_mix_ind.png"),
                          plot     = p))
  
  
  # plot vert ---------------------------------------------------------------
  p <- f.vis_nais_si_vert_dclass(np  = nais_profile,
                                 st  = stand,
                                 i_f = ind_full,
                                 tc  = theme_custom)
  suppressMessages(ggsave(filename = str_c(out_folder, "2a_vert_dclass.png"),
                          plot     = p))
  if (natural_hazard == "RF") {
    p <- f.vis_nais_si_vert_stand_RF(np = nais_profile,
                                     st = stand,
                                     tc = theme_custom)
    suppressMessages(ggsave(filename = str_c(out_folder, "2b_vert_RF_stand.png"),
                            plot     = p))
    
    p <- f.vis_nais_si_vert_ind_RF(i_f = ind_full,
                                   tc  = theme_custom)
    suppressMessages(ggsave(filename = str_c(out_folder, "2c_vert_RF_ind.png"),
                            plot     = p))
  }
  p <- f.vis_nais_si_vert_ind(i_f = ind_full,
                              tc  = theme_custom)
  suppressMessages(ggsave(filename = str_c(out_folder, "2d_vert_ind.png"),
                          plot     = p))
  
  
  # plot horiz --------------------------------------------------------------
  if (natural_hazard == "A") {
    p <- f.vis_nais_si_horiz_stand_A(st = stand,
                                     np = nais_profile,
                                     tc = theme_custom)
    suppressMessages(ggsave(filename = str_c(out_folder, "3a_horiz_A_stand.png"),
                            plot     = p))
  } else if (natural_hazard == "LED") {
    p <- f.vis_nais_si_horiz_stand_LED(st = stand,
                                       np = nais_profile,
                                       tc = theme_custom)
    suppressMessages(ggsave(filename = str_c(out_folder, "3a_horiz_LED_stand.png"),
                            plot     = p))
  } else if (natural_hazard == "RF") {
    p <- f.vis_nais_si_horiz_stand_RF(st = stand,
                                      np = nais_profile,
                                      tc = theme_custom)
    suppressMessages(ggsave(filename = str_c(out_folder, "3a_horiz_RF_stand.png"),
                            plot     = p))
  } else if (natural_hazard == "TF") {
    p <- f.vis_nais_si_horiz_stand_TF(np = nais_profile,
                                      st = stand,
                                      tc = theme_custom)
    suppressMessages(ggsave(filename = str_c(out_folder, "3a_horiz_TF_stand.png"),
                            plot     = p))
  }
  
  p <- f.vis_nais_si_horiz_ind(i_f = ind_full,
                               tc  = theme_custom)
  suppressMessages(ggsave(filename = str_c(out_folder, "3b_horiz_ind.png"),
                          plot     = p))
  
  
  # plot supptr -------------------------------------------------------------
  p <- f.vis_nais_si_supptr(np  = nais_profile,
                            st  = stand,
                            tc  = theme_custom,
                            i_f = ind_full)
  suppressMessages(ggsave(filename = str_c(out_folder, "4_supptr.png"),
                          plot     = p))
  
  
  # plot seedl --------------------------------------------------------------
  p <- f.vis_nais_si_seedl(np = nais_profile,
                           st = stand,
                           tc = theme_custom,
                           i_f = ind_full)
  suppressMessages(ggsave(filename = str_c(out_folder, "5_seedl.png"),
                          plot     = p))
  
  
  # plot sapthi -------------------------------------------------------------
  p <- f.vis_nais_si_sapthi_stand(np = nais_profile,
                                  st = stand,
                                  tc = theme_custom)
  suppressMessages(ggsave(filename = str_c(out_folder, "6a_sapthi_stand.png"),
                          plot     = p))
  
  p <- f.vis_nais_si_sapthi_ind(i_f = ind_full,
                                tc  = theme_custom)
  suppressMessages(ggsave(filename = str_c(out_folder, "6b_sapthi_ind.png"),
                          plot     = p))
}
