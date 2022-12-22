######################
#       ASSESS       #
# PROTECTION QUALITY #
# OF SIMULATED STAND #
#     07.06.2022     #
######################


PROFORM_assess_NaiS <- function(simulation, stratum_fine,
                                 natural_hazard, rockfall_scenario,
                                 time_resolution, time_start, time_end) {
  # setup -------------------------------------------------------------------
  suppressWarnings(suppressMessages(library(tidyverse)))
  suppressWarnings(suppressMessages(library(magrittr)))
  
  
  # load data ---------------------------------------------------------------
  param <- read_rds(str_c("data/processed/simoutput/", simulation, "/3_output/param.rds"))
  coordinates <- read_rds(str_c("data/processed/simoutput/", simulation, "/3_output/coordinates.rds"))
  res_full <- read_rds(str_c("data/processed/simoutput/", simulation, "/3_output/res_full.rds"))
  res_stand <- read_rds(str_c("data/processed/simoutput/", simulation, "/3_output/res_standdev.rds"))
  
  
  # load functions and nais-profile -----------------------------------------
  source("code/nais/functions_nais.R", local = TRUE)
  source("code/nais/nais_profiles.R", local = TRUE)
  
  
  # prepare static data -----------------------------------------------------
  # hexagon metrics
  flat_hex <- f.hex_param(param$sim_area_m2, param$sim_ncells)
  
  # time steps to calculate indices
  if(is.na(time_end)) {
    time_end <- param$sim_time
  }
  
  check_years <- seq(from = time_start,
                     to = time_end,
                     by = time_resolution)
  
  
  if (tail(check_years, 1) != time_end) {
    check_years <- c(check_years, time_end)
  }
  
  # cell info
  cell_info <- res_full %>% 
    filter(time_Step == 0) %>% 
    select(starts_with("cc_")) %>% 
    distinct()
  
  # combined stand results
  res_stand <- res_stand %>% 
    filter(State_nr == 1) %>% 
    filter(time_Step %in% check_years) %>% 
    mutate(Species = factor(Species,
                            levels = c("none", "aalb", "apse", "fsyl", "pabi"))) %>% 
    left_join(cell_info, by = "cc_ID")
  
  # combined stand results of largest cohort (lc)
  res_stand_lc <- res_stand %>%
    group_by(time_Step, cc_ID) %>% 
    slice_max(order_by  = D,
              n         = 1,
              with_ties = FALSE) %>% 
    ungroup()
  
  # prepare data structure to save results
  nais_indices_full <- vector(mode = "list", length = length(check_years))
  nais_stand <- vector(mode = "list", length = length(check_years))
  
  # loop over years ---------------------------------------------------------
  for (i in seq_along(check_years)) {
    cat("\r assessing step ", i, "/", length(check_years), sep = "")
    
    year <- check_years[i]
    
    
    # prepare step data -------------------------------------------------------
    step_df <- res_stand %>% 
      filter(time_Step == year)
    
    step_df_lc <- res_stand_lc %>% 
      filter(time_Step == year)
    
    nais_indices_year <- list()
    nais_stand_year <- list()
    
    
    # gaps: identification and metrics ----------------------------------------
    if (natural_hazard != "none") {
      gap_df <- f.gap_identify(df_step_red = step_df_lc)
      
      # depending on natural hazard, calculate only necessary metrics
      if (natural_hazard == "A") {
        gapmetrics_cell <- f.gap_metrics_cell(df_gaps = gap_df,
                                              coor    = coordinates,
                                              area    = FALSE,
                                              length  = TRUE,
                                              width   = TRUE)
        gapmetrics_m <- f.gap_metrics_m(gm_raw        = gapmetrics_cell,
                                        hex_param     = flat_hex,
                                        slope_deg     = param$sim_slope,
                                        area          = FALSE,
                                        length        = TRUE,
                                        width         = TRUE)
      } else if (natural_hazard == "LED") {
        gapmetrics_cell <- f.gap_metrics_cell(df_gaps = gap_df,
                                              coor    = coordinates,
                                              area    = TRUE,
                                              length  = FALSE,
                                              width   = TRUE)
        gapmetrics_m <- f.gap_metrics_m(gm_raw        = gapmetrics_cell,
                                        hex_param     = flat_hex,
                                        slope_deg     = param$sim_slope,
                                        area          = TRUE,
                                        length        = FALSE,
                                        width         = TRUE)
      } else if (natural_hazard == "RF") {
        gapmetrics_cell <- f.gap_metrics_cell(df_gaps = gap_df,
                                              coor    = coordinates,
                                              area    = FALSE,
                                              length  = TRUE,
                                              width   = FALSE)
        gapmetrics_m <- f.gap_metrics_m(gm_raw        = gapmetrics_cell,
                                        hex_param     = flat_hex,
                                        slope_deg     = param$sim_slope,
                                        area          = FALSE,
                                        length        = TRUE,
                                        width         = FALSE)
      } else if (natural_hazard == "TF") {
        gapmetrics_cell <- f.gap_metrics_cell(df_gaps = gap_df,
                                              coor    = coordinates,
                                              area    = TRUE,
                                              length  = TRUE,
                                              width   = FALSE)
        gapmetrics_m <- f.gap_metrics_m(gm_raw        = gapmetrics_cell,
                                        hex_param     = flat_hex,
                                        slope_deg     = param$sim_slope,
                                        area          = TRUE,
                                        length        = TRUE,
                                        width         = FALSE)
      }
    }
    
    
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
    }
    
    # vertical arrangement overall
    nais_indices_year$vert <- f.nais_vert(indices_list = nais_indices_year)
    
    
    # horizontal arrangement --------------------------------------------------
    if (natural_hazard == "none") {
      nais_indices_year <- c(nais_indices_year, list(gap   = NA,
                                                     gap_l = NA,
                                                     gap_a = NA,
                                                     cc    = NA,
                                                     horiz = NA))
      
    } else if (natural_hazard == "A") {
      temp <- f.nais_A(df_step_red = step_df_lc,
                       gapmetrics  = gapmetrics_m,
                       nais_pr     = nais_profile$A,
                       cc_red      = nais_profile$cc_red)
      nais_indices_year <- c(nais_indices_year, temp$res)
      nais_stand_year <- c(nais_stand_year, temp$stand)
      
    } else if (natural_hazard == "LED") {
      temp <- f.nais_LED(df_step     = step_df,
                         df_step_red = step_df_lc,
                         gapmetrics  = gapmetrics_m,
                         gaps_cells  = gap_df,
                         nais_pr_led = nais_profile$LED,
                         nais_pr_st  = nais_profile$sapthi,
                         cc_red      = nais_profile$cc_red)
      nais_indices_year <- c(nais_indices_year, temp$res)
      nais_stand_year <- c(nais_stand_year, temp$stand)
      
    } else if (natural_hazard == "RF") {
      temp <- f.nais_RF_gap(gapmetrics = gapmetrics_m,
                            nais_pr    = nais_profile$RF)
      nais_indices_year <- c(nais_indices_year, temp$res)
      nais_stand_year <- c(nais_stand_year, temp$stand)
      
    } else if (natural_hazard == "TF") {
      temp <- f.nais_TF(df_step_red = step_df_lc,
                        gapmetrics  = gapmetrics_m,
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
  
  nais_indices_full <- bind_rows(nais_indices_full) %>% 
    mutate(simulation        = simulation,
           natural_hazard    = natural_hazard,
           rockfall_scenario = rockfall_scenario) %>% 
    select(simulation, natural_hazard, rockfall_scenario, time_Step, everything())
  
  nais_stand <- bind_rows(nais_stand) %>% 
    mutate(simulation        = simulation,
           natural_hazard    = natural_hazard,
           rockfall_scenario = rockfall_scenario) %>% 
    select(simulation, natural_hazard, rockfall_scenario, time_Step, everything())
  
  
  # reduce index list -------------------------------------------------------
  nais_indices_red <- select(nais_indices_full,
                             simulation, natural_hazard, rockfall_scenario, time_Step,
                             mix, vert, horiz, supptr, seedl, sapthi)
  
  
  # save data ---------------------------------------------------------------
  # create output folder
  if (natural_hazard == "RF") {
    output_folder <- str_c("data/processed/naisoutput/", simulation, "_", natural_hazard, "_", rockfall_scenario, "/")
  } else {
    output_folder <- str_c("data/processed/naisoutput/", simulation, "_", natural_hazard, "/")
  }
  dir.create(output_folder, showWarnings = FALSE)
  
  
  # input infos
  info <- list()
  info$simulation <- simulation
  info$stratum <- param$sim_stratum
  info$stratum_fine <- stratum_fine
  info$natural_hazard <- natural_hazard
  info$rockfall_scenario <- rockfall_scenario
  info$time_resolution <- time_resolution
  info$timestamp <- date()
  info <- bind_rows(info) %>% 
    mutate(across(everything(), as.character)) %>% 
    pivot_longer(everything(), names_to = "var", values_to = "val") %>% 
    as.data.frame()
  
  descr_file <- str_c(output_folder, "info.txt")
  capture.output(info,
                 file = descr_file,
                 type = "output")
  
  # comparison results
  saveRDS(nais_indices_full,
          str_c(output_folder, "nais_indices_full.rds"))
  saveRDS(nais_indices_red,
          str_c(output_folder, "nais_indices_red.rds"))
  saveRDS(nais_stand,
          str_c(output_folder, "nais_stand.rds"))
  
  cat("\n")
}
