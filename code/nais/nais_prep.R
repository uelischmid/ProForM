######################
#      PREPARE       #
# PROTECTION QUALITY #
#     ASSESSMENT     #
# OF SIMULATED STAND #
#     09.12.2022     #
######################

PROFORM_NaiS_prep <- function (simulation,
                               t_start, t_end, t_res,
                               out_suffix) {
  
  # load library, data and functions ----------------------------------------
  suppressWarnings(suppressMessages(library(tidyverse)))
  cat(" load data")
  
  param <- read_rds(str_c("data/processed/simoutput/", simulation, "/3_output/param.rds"))
  coordinates <- read_rds(str_c("data/processed/simoutput/", simulation, "/3_output/coordinates.rds"))
  res_full <- read_rds(str_c("data/processed/simoutput/", simulation, "/3_output/res_full.rds"))
  res_stand <- read_rds(str_c("data/processed/simoutput/", simulation, "/3_output/res_standdev.rds"))
  
  source("code/nais/functions_nais.R", local = TRUE)
  
  
  # get stand data ----------------------------------------------------------
  cat("\n data preparation\n")
  
  # hexagon metrics
  flat_hex <- f.hex_param(param$sim_area_m2, param$sim_ncells)
  
  # time steps to calculate indices
  if(is.na(t_end)) {
    t_end <- param$sim_time
  }
  
  check_years <- seq(from = t_start,
                     to   = t_end,
                     by   = t_res)
  
  if (tail(check_years, 1) != t_end) {
    check_years <- c(check_years, t_end)
  }
  
  # cell info
  cell_info <- res_full %>% 
    filter(time_Step == 0) %>% 
    select(starts_with("cc_")) %>% 
    distinct()
  
  # combined stand results
  res_stand_red <- res_stand %>% 
    filter(State_nr == 1) %>% 
    filter(time_Step %in% check_years) %>% 
    mutate(Species = factor(Species,
                            levels = c("none", "aalb", "apse", "fsyl", "pabi"))) %>% 
    left_join(cell_info, by = "cc_ID")
  
  # combined stand results of largest cohort (lc)
  res_stand_red_lc <- res_stand_red %>%
    group_by(time_Step, cc_ID) %>% 
    slice_max(order_by  = D,
              n         = 1,
              with_ties = FALSE) %>% 
    ungroup()
  
  # groups of 3 cells for gap analysis
  groups_3c <- f.gap_build_groups_3c(c_i = cell_info)
  
  
  # identify and characterize gaps ------------------------------------------
  gap_data <- vector(mode = "list", length = length(check_years))
  
  for (i in seq_along(check_years)) {
    cat("\r gap analysis of step ", i, "/", length(check_years), sep = "")
    
    # prepare step data
    year <- check_years[i]
    step_df <- res_stand_red %>% 
      filter(time_Step == year)
    
    step_df_lc <- res_stand_red_lc %>% 
      filter(time_Step == year)
    
    # identify gaps
    gap_df <- f.gap_identify_3c(groups       = groups_3c,
                                df_step_red = step_df_lc)
    
    # calculate gap metrics
    gapmetrics_cell <- f.gap_metrics_cell(df_gaps = gap_df,
                                          coor    = coordinates)
    gapmetrics_m <- f.gap_metrics_m(gm_raw    = gapmetrics_cell,
                                    hex_param = flat_hex,
                                    slope_deg = param$sim_slope)
    # combine gap data
    gap_data[[i]] <- gap_df %>% 
      group_by(gap_nr) %>% 
      nest() %>% 
      mutate(cc_IDs = map(data, 1)) %>% 
      select(-data) %>% 
      left_join(gapmetrics_cell, by = "gap_nr") %>% 
      left_join(gapmetrics_m, by = "gap_nr") %>% 
      mutate(simulation = simulation,
             time_Step  = year) %>% 
      select(simulation, time_Step, everything())
  }
  cat("\n save data")
  
  gap_data <- bind_rows(gap_data)
  
  
  # save data ---------------------------------------------------------------
  if (is.na(out_suffix)) {
    output_folder <- str_c("data/processed/naisoutput/1_prep/", simulation, "/")
  } else {
    output_folder <- str_c("data/processed/naisoutput/1_prep/", simulation, "_", out_suffix, "/")
  }
  
  dir.create(output_folder, showWarnings = FALSE)
  
  # input info
  tibble(simulation = simulation,
         t_start    = t_start,
         t_end      = t_end,
         t_res      = t_res,
         timestamp  = date()) %>% 
    write_csv(file = str_c(output_folder, "0_info.csv"))
  
  # reduced stand results
  write_rds(res_stand_red,
            file = str_c(output_folder, "res_stand_red.rds"))
  write_rds(res_stand_red_lc,
            file = str_c(output_folder, "res_stand_red_lc.rds"))
  
  # gap info
  write_rds(gap_data,
            file = str_c(output_folder, "gap_data.rds"))
  
  # parameters
  write_rds(param,
            file = str_c(output_folder, "param_sim.rds"))
  cat("\n")
}

