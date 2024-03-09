#####
# visualize nais assessment (relaxed)
# all subindices
# mgm and corresponding NOM simulation
# 3.1.24, us
#####

PROFORM_vis_2_nais_si_relaxed_c3 <- function(nais_assessment_mgm, out_subfolder) {
  
  # setup -------------------------------------------------------------------
  suppressWarnings(suppressMessages(library(tidyverse)))
  suppressWarnings(suppressMessages(library(cowplot)))
  
  
  # get NOM-simulation and combination name ---------------------------------
  nais_assessment_nom <- nais_assessment_mgm %>% 
    str_split("_") %>% 
    magrittr::extract2(1)
  
  if (nais_assessment_nom[2] == "ST") {
    nais_assessment_nom[7] <- "NOM"
    nais_assessment_nom[8] <- "t0"
    nais_assessment_nom[9] <- "i0"
  } else if (nais_assessment_nom[2] == "LT") {
    nais_assessment_nom[5] <- "NOM"
    nais_assessment_nom[6] <- "t0"
    nais_assessment_nom[7] <- "i0"
  } else {
    cat("ERROR: only c3 assessments allowed")
    quit(save = "ask")
  }
  
  nais_assessment_nom <- str_c(nais_assessment_nom, collapse = "_")
  
  
  # load assessment, profile & plot functions -------------------------------
  folder_mgm <- str_c("data/processed/naisoutput/2_assessment_relaxed/", nais_assessment_mgm, "/")
  folder_nom <- str_c("data/processed/naisoutput/2_assessment_relaxed/", nais_assessment_nom, "/")
  
  ind_full <- vector(mode = "list", length = 2)
  ind_full[[1]] <- read_rds(str_c(folder_mgm, "nais_indices_full.rds")) %>% 
    mutate(name = "mgm")
  ind_full[[2]] <- read_rds(str_c(folder_nom, "nais_indices_full.rds")) %>% 
    mutate(name = "NOM")
  ind_full <- bind_rows(ind_full) %>% 
    mutate(name = factor(name, levels = c("mgm", "NOM")))
  
  ind_red <- vector(mode = "list", length = 2)
  ind_red[[1]] <- read_rds(str_c(folder_mgm, "nais_indices_red.rds")) %>% 
    mutate(name = "mgm")
  ind_red[[2]] <- read_rds(str_c(folder_nom, "nais_indices_red.rds")) %>% 
    mutate(name = "NOM")
  ind_red <- bind_rows(ind_red) %>% 
    mutate(name = factor(name, levels = c("mgm", "NOM")))
  
  stand <- vector(mode = "list", length = 2)
  stand[[1]] <- read_rds(str_c(folder_mgm, "nais_stand.rds")) %>% 
    mutate(name = "mgm")
  stand[[2]] <- read_rds(str_c(folder_nom, "nais_stand.rds")) %>% 
    mutate(name = "NOM")
  stand <- bind_rows(stand) %>% 
    mutate(name = factor(name, levels = c("mgm", "NOM")))
  
  nais_profile <- read_rds(str_c(folder_mgm, "nais_profile.rds"))
  nais_profile_nom <- read_rds(str_c(folder_nom, "nais_profile.rds"))
  
  if (identical(nais_profile, nais_profile_nom) == FALSE) {
    cat("ERROR: nais profiles not identical")
    quit(save = "ask")
  } else {
    rm(nais_profile_nom)
  }
  natural_hazard <- unique(ind_full$natural_hazard)
  
  source("code/visualization/c3/c3_vis_functions.R", local = TRUE)
  
  
  # create or empty folder --------------------------------------------------
  out_folder <- "results/nais_vis/1_sim_subind/"
  if (!is.na(out_subfolder)){
    out_folder <- str_c(out_folder, out_subfolder, "/")
  }
  
  out_folder <- str_c(out_folder, nais_assessment_mgm, "/")
  if (dir.exists(out_folder)) {
    unlink(str_c(out_folder, "*"), recursive = TRUE)
  } else {
    dir.create(out_folder, recursive = TRUE, showWarnings = TRUE)
  }
  

  # plot F2 -----------------------------------------------------------------
  p <- f.vis_F2_2ass_c3(i_r = ind_red,
                        capt = nais_assessment_mgm)
  suppressWarnings(ggsave(filename = str_c(out_folder, "0_F2.jpg"),
                          width = 5,
                          height = 15,
                          units = "cm",
                          scale = 2))
  
  
  # plot mix ----------------------------------------------------------------
  p <- f.vis_nais_si_mix_stand_2ass_c3(st = stand,
                                       np = nais_profile)
  suppressMessages(ggsave(filename = str_c(out_folder, "1a_mix_stand.jpg"),
                          plot     = p))
  
  p <- f.vis_nais_si_mix_ind_2ass_c3(i_f = ind_full)
  suppressMessages(ggsave(filename = str_c(out_folder, "1b_mix_ind.jpg"),
                          plot     = p))
  
  
  # plot vert ---------------------------------------------------------------
  p <- f.vis_nais_si_vert_dclass_2ass_c3(np  = nais_profile,
                                         st  = stand,
                                         i_f = ind_full)
  suppressMessages(ggsave(filename = str_c(out_folder, "2a_vert_dclass.jpg"),
                          plot     = p))
  
  p <- f.vis_nais_si_vert_ind_2ass_c3(i_f = ind_full)
  suppressMessages(ggsave(filename = str_c(out_folder, "2b_vert_ind.jpg"),
                          plot     = p))
  
  
  # plot horiz --------------------------------------------------------------
  if (natural_hazard == "A") {
    p <- f.vis_nais_si_horiz_stand_A_2ass_c3(st = stand,
                                             np = nais_profile)
    suppressMessages(ggsave(filename = str_c(out_folder, "3a_horiz_A_stand.jpg"),
                            plot     = p))
  } else if (natural_hazard == "LED") {
    p <- f.vis_nais_si_horiz_stand_LED_2ass_c3(st = stand,
                                               np = nais_profile)
    suppressMessages(ggsave(filename = str_c(out_folder, "3a_horiz_LED_stand.jpg"),
                            plot     = p))
  }
  
  p <- f.vis_nais_si_horiz_ind_2ass_c3(i_f = ind_full)
  suppressMessages(ggsave(filename = str_c(out_folder, "3b_horiz_ind.jpg"),
                          plot     = p))
  
  
  # plot supptr -------------------------------------------------------------
  p <- f.vis_nais_si_supptr_2ass_c3(np  = nais_profile,
                                    st  = stand,
                                    i_f = ind_full)
  suppressMessages(ggsave(filename = str_c(out_folder, "4_supptr.jpg"),
                          plot     = p))
  
  
  # plot seedl --------------------------------------------------------------
  p <- f.vis_nais_si_seedl_2ass_c3(np  = nais_profile,
                                   st  = stand,
                                   i_f = ind_full)
  suppressMessages(ggsave(filename = str_c(out_folder, "5_seedl.jpg"),
                          plot     = p))
  
  
  # plot sapthi -------------------------------------------------------------
  p <- f.vis_nais_si_sapthi_stand_2ass_c3(np = nais_profile,
                                          st = stand,)
  suppressMessages(ggsave(filename = str_c(out_folder, "6a_sapthi_stand.jpg"),
                          plot     = p))
  
  p <- f.vis_nais_si_sapthi_ind_2ass_c3(i_f = ind_full)
  suppressMessages(ggsave(filename = str_c(out_folder, "6b_sapthi_ind.jpg"),
                          plot     = p))
}
