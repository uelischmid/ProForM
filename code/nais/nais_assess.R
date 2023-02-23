######################
# PROTECTION QUALITY #
#     ASSESSMENT     #
# OF SIMULATED STAND #
#     09.12.2022     #
######################

PROFORM_NaiS_assess <- function (nais_prep,
                                 natural_hazard,
                                 rockfall_scenario) {
  
  # load library, data and functions ----------------------------------------
  cat(" load data\n")
  
  suppressWarnings(suppressMessages(library(tidyverse)))

  folder_in <- str_c("data/processed/naisoutput/1_prep/", nais_prep, "/")
  
  prep_info <- read_csv(str_c(folder_in, "0_info.csv"),
                        col_types = cols())
  res_stand_red <- read_rds(str_c(folder_in, "res_stand_red.rds"))
  res_stand_red_lc <- read_rds(str_c(folder_in, "res_stand_red_lc.rds"))
  gap_data <- read_rds(str_c(folder_in, "gap_data.rds"))
  param <- read_rds(str_c(folder_in, "param_sim.rds"))
  
  source("code/nais/functions_nais.R", local = TRUE)
  source("code/nais/nais_profiles.R", local = TRUE)
  
  # prepare data structure to save results
  check_years <- sort(unique(res_stand_red$time_Step))
  nais_indices_full <- vector(mode = "list", length = length(check_years))
  nais_stand <- vector(mode = "list", length = length(check_years))
  
  # loop over years ---------------------------------------------------------
  for (i in seq_along(check_years)) {
    cat("\r assessment of step ", i, "/", length(check_years), sep = "")
    
    # prepare step data -------------------------------------------------------
    year <- check_years[i]
    
    step_df <- res_stand_red %>% 
      filter(time_Step == year)
    
    step_df_lc <- res_stand_red_lc %>% 
      filter(time_Step == year)
    
    gap_metrics <- gap_data %>% 
      filter(time_Step == year)
    
    gap_cc_IDs <- gap_metrics %>% 
      select(gap_nr, cc_IDs) %>% 
      unnest(cc_IDs) %>% 
      rename(cc_ID = cc_IDs)
    
    nais_indices_year <- list()
    nais_stand_year <- list()
    
    
    # mixture -----------------------------------------------------------------
    temp <- f.nais_mixture(df_step = step_df,
                           nais_pr = nais_profile$mixture)
    nais_indices_year <- c(nais_indices_year, temp$res)
    nais_stand_year <- c(nais_stand_year, temp$stand)
    
    
    # vertical arrangement ----------------------------------------------------
    # diameter classes
    temp <- f.nais_dclass(df_step = step_df,
                          nais_pr = nais_profile$va_dc)
    nais_indices_year <- c(nais_indices_year, temp$res)
    nais_stand_year <- c(nais_stand_year, temp$stand)
    
    # stem numbers and basal area
    if (natural_hazard == "RF") {
      temp <- f.nais_RF_nba(df_step = step_df,
                            nais_pr = nais_profile$RF)
      nais_indices_year <- c(nais_indices_year, temp$res)
      nais_stand_year <- c(nais_stand_year, temp$stand)
    } else {
      nais_indices_year <- c(nais_indices_year, list(sn_N1 = NA,
                                                     sn_N2 = NA,
                                                     sn_N3 = NA,
                                                     sn_N4 = NA,
                                                     sn    = NA,
                                                     ba_ha = NA))
      nais_stand_year <- c(nais_stand_year, list(sn_N1 = NA,
                                                 sn_N2 = NA,
                                                 sn_N3 = NA,
                                                 sn_N4 = NA,
                                                 ba_ha = NA))
    }
    
    # vertical arrangement overall
    nais_indices_year$vert <- f.nais_vert(indices_list = nais_indices_year)
    
    
    # horizontal arrangement --------------------------------------------------
    if (natural_hazard == "A") {
      temp <- f.nais_A(df_step_red = step_df_lc,
                       gapmetrics  = gap_metrics,
                       nais_pr     = nais_profile$A,
                       cc_red      = nais_profile$cc_red)
      nais_indices_year <- c(nais_indices_year, temp$res)
      nais_stand_year <- c(nais_stand_year, temp$stand)
      
    } else if (natural_hazard == "LED") {
      temp <- f.nais_LED(df_step     = step_df,
                         df_step_red = step_df_lc,
                         gapmetrics  = gap_metrics,
                         gaps_cells  = gap_cc_IDs,
                         nais_pr_led = nais_profile$LED,
                         nais_pr_st  = nais_profile$sapthi,
                         cc_red      = nais_profile$cc_red)
      nais_indices_year <- c(nais_indices_year, temp$res)
      nais_stand_year <- c(nais_stand_year, temp$stand)
      
    } else if (natural_hazard == "RF") {
      temp <- f.nais_RF_gap(gapmetrics = gap_metrics,
                            nais_pr    = nais_profile$RF)
      nais_indices_year <- c(nais_indices_year, temp$res)
      nais_stand_year <- c(nais_stand_year, temp$stand)
      
    } else if (natural_hazard == "TF") {
      temp <- f.nais_TF(df_step_red = step_df_lc,
                        gapmetrics  = gap_metrics,
                        nais_pr     = nais_profile$TF,
                        cc_red      = nais_profile$cc_red)
      nais_indices_year <- c(nais_indices_year, temp$res)
      nais_stand_year <- c(nais_stand_year, temp$stand)
    }
    
    
    # support trees -----------------------------------------------------------
    temp <- f.nais_cr(df_step = step_df,
                      nais_pr = nais_profile$suptr_cr)
    nais_indices_year <- c(nais_indices_year, temp$res)
    nais_stand_year <- c(nais_stand_year, temp$stand)
    
    
    # regeneration: seedlings -------------------------------------------------
    temp <- f.nais_seedl(df_step     = step_df,
                         df_step_red = step_df_lc,
                         nais_pr     = nais_profile$seedl)
    nais_indices_year <- c(nais_indices_year, temp$res)
    nais_stand_year <- c(nais_stand_year, temp$stand)
    
    
    # regeneration: saplings & thicket ----------------------------------------
    temp <- f.nais_sapthi(appl    = "stand",
                          df_step = step_df,
                          nais_pr = nais_profile$sapthi)
    nais_indices_year <- c(nais_indices_year, temp$res)
    nais_stand_year <- c(nais_stand_year, temp$stand)
    
    
    # combine results ---------------------------------------------------------
    nais_indices_full[[i]] <- as_tibble(nais_indices_year) %>% 
      mutate(time_Step = year)
    
    nais_stand[[i]] <- as_tibble(nais_stand_year) %>% 
      mutate(time_Step = year)
  }
  

  # combine results ---------------------------------------------------------
  nais_indices_full <- bind_rows(nais_indices_full) %>% 
    mutate(simulation        = param$sim_name,
           nais_prep         = nais_prep,
           stratum           = param$sim_stratum,
           natural_hazard    = natural_hazard,
           rockfall_scenario = rockfall_scenario) %>% 
    select(simulation, nais_prep, stratum,
           natural_hazard, rockfall_scenario,
           time_Step, everything())
  
  nais_stand <- bind_rows(nais_stand) %>% 
    mutate(simulation        = param$sim_name,
           nais_prep         = nais_prep,
           stratum           = param$sim_stratum,
           natural_hazard    = natural_hazard,
           rockfall_scenario = rockfall_scenario) %>% 
    select(simulation, nais_prep, stratum,
           natural_hazard, rockfall_scenario,
           time_Step, everything())
  
  nais_indices_red <- nais_indices_full %>% 
    select(simulation, nais_prep, stratum,
           natural_hazard, rockfall_scenario,
           time_Step,
           mix, vert, horiz, supptr, seedl, sapthi)
  
  
  # save data ---------------------------------------------------------------
  cat("\n save data")
  
  # create output folder
  if (natural_hazard == "RF") {
    output_folder <- str_c("data/processed/naisoutput/2_assessment/", nais_prep, "_", natural_hazard, rockfall_scenario, "/")
  } else {
    output_folder <- str_c("data/processed/naisoutput/2_assessment/", nais_prep, "_", natural_hazard, "/")
  }
  dir.create(output_folder, showWarnings = FALSE)
  
  # input infos
  tibble(simulation        = param$sim_name,
         nais_prep         = nais_prep,
         stratum           = param$sim_stratum,
         natural_hazard    = natural_hazard,
         rockfall_scenario = rockfall_scenario,
         timestamp         = date()) %>% 
    write_csv(file = str_c(output_folder, "0_info.csv"))
  
  # nais profile
  saveRDS(nais_profile,
          str_c(output_folder, "nais_profile.rds"))
  
  # comparison results
  saveRDS(nais_indices_full,
          str_c(output_folder, "nais_indices_full.rds"))
  saveRDS(nais_indices_red,
          str_c(output_folder, "nais_indices_red.rds"))
  saveRDS(nais_stand,
          str_c(output_folder, "nais_stand.rds"))
  
  cat("\n")
}
