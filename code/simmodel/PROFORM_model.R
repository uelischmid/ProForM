######################
##     PROFORM      ##
## simulation model ##
######################


PROFORM_simulate <- function(simsettings,
                             input_folder  = "data/processed/",
                             output_folder = "data/processed/simoutput/") {
  ## Inputs
  # simsettings: name of .csv-file with simulation settings
  # input_folder: folder where input data is located
  # output_folder: folder where output data should be saved
  
  ## Outputs
  # Various simulation outputs
  
  
  # load packages -----------------------------------------------------------
  suppressWarnings(suppressMessages(library(tidyverse)))
  
  # init: load data ---------------------------------------------------------
  cat("----INITIALISE MODEL----\n")
  cat("simulation settings:", simsettings, "\n")
  
  ## load parameter functions
  source("code/simmodel/functions_init.R", local = TRUE)
  
  ## load simulation settings 
  param_sim <- f.init_load_param_sim()
  
  ## load init data
  init_data <- f.init_load_init_data()
  
  ## load forest dynamics parameters
  param_fordyn <- f.init_load_param_fordyn()
  species_parametrized <- f.init_identify_spc()
  
  ## load management parameters
  mgm_instructions <- f.init_load_mgm()
  
  ## calculate further sim-parameters
  param_sim <- f.init_calculate_param()
  param_fordyn$str_cellsize_all <- NULL
  
  ## combine parameters
  param <- c(param_sim, param_fordyn)
  rm(param_sim, param_fordyn)
  
  ## load remaining functions
  source("code/simmodel/functions_aux.R", local = TRUE)
  source("code/simmodel/functions_dyn_flag.R", local = TRUE)
  source("code/simmodel/functions_update.R", local = TRUE)
  source("code/simmodel/functions_mgm.R", local = TRUE)
  source("code/simmodel/functions_results.R", local = TRUE)
  
  ## set random seed
  set.seed(param$sim_seed)
  
  # init: check data --------------------------------------------------------
  datacheck <- f.init_check_data()
  
  # init: simulation grid ---------------------------------------------------
  cat("----CALCULATE COORDINATES & NEIGHBORS----\n")
  coordinates <- f.init_grid()
  
  # init: neighbors ---------------------------------------------------------
  neighbor_ids_vec <- f.init_neighbors()
  
  # init: prepare data structures -------------------------------------------
  # list for storing results
  res_list <- vector(mode = "list", length = param$sim_time + 1)
  
  # data frame for iterative calculations
  step_df <- f.init_create_simtable()
  
  cat("----SIMULATION----\n")
  
  for (i in 0:param$sim_time) {
    ### time_Step i
    cat("\r    simulating time step ", i, "/", param$sim_time, sep = "")
    
    # sim: calculate s2-auxiliary variables -----------------------------------
    # cell stage
    step_df$aux2_Stage <- map2_dbl(step_df$s2_H, step_df$s2_D,
                                   f.aux_stage)
    
    # basal area
    step_df$aux2_BA <- map2_dbl(step_df$s2_N, step_df$s2_D,
                                f.aux_ba)
    
    # stem volume
    step_df$aux2_Vol <- map2_dbl(step_df$s2_N, step_df$s2_D,
                                 f.aux_vol)
    
    # crown ratio
    step_df$aux2_CR <- map2_dbl(step_df$s2_H, step_df$s2_HCB,
                                f.aux_cr)
    
    # sim: management ---------------------------------------------------------
    # marking of trees (mgm_N)
    if (unique(step_df$time_Step) %in% param$mgm_interv_steps) {
      if (param$mgm_type == "RDC_tree") {
        step_df <- f.mgm_RDC_tree()
      } else if (param$mgm_type == "RDC_cohort") {
        step_df <- f.mgm_RDC_cohort()
      }
    } else {
      step_df$mgm_N <- 0
    }
    
    # mortality flag of entire cell (flag_CohortDeath_mgm)
    step_df$flag_CohortDeath_mgm <- pmap_lgl(list(N     = step_df$s2_N,
                                                  dyn_N = step_df$mgm_N),
                                             f.flag_cohortdeath)
    
    # advance regeneration (flag_AdReg_mgm)
    step_df$flag_AdReg_mgm <- map2_lgl(step_df$aux2_Stage,
                                       step_df$flag_CohortDeath_mgm,
                                       f.flag_adreg)
    
    # sim: save table to results ----------------------------------------------
    res_list[[i + 1]] <- step_df
    
    ### time_Step i + 1
    # time step
    step_df$time_Step <- step_df$time_Step + 1
    
    # sim: update state 1 -----------------------------------------------------
    # species
    temp_spc_selection_probs <- f.aux_spcprob(Species = step_df$s2_Spc,
                                              D       = step_df$s2_D,
                                              BA      = step_df$aux2_BA)
    
    step_df$s1_Spc <- pmap_chr(list(Spc_previous     = step_df$s2_Spc,
                                    flag_Germ        = FALSE,
                                    flag_CohortDeath = step_df$flag_CohortDeath_mgm,
                                    flag_AdReg       = step_df$flag_AdReg_mgm),
                               f.update_Spc,
                               spc_selection_probs = temp_spc_selection_probs)
    
    # stem number (s1_N)
    step_df$s1_N <- pmap_dbl(list(N_previous = step_df$s2_N,
                                  dyn_Ningr  = 0,
                                  dyn_Nmort  = 0,
                                  mgm_N      = step_df$mgm_N),
                             f.update_N)
    
    # tree diameter (s1_D)
    step_df$s1_D <- pmap_dbl(list(D_previous       = step_df$s2_D,
                                  dyn_D            = 0,
                                  flag_CohortDeath = step_df$flag_CohortDeath_mgm),
                             f.update_D)
    
    # tree height (s1_H)
    step_df$s1_H <- pmap_dbl(list(H_previous       = step_df$s2_H,
                                  dyn_H            = 0,
                                  flag_Germ        = FALSE,
                                  flag_CohortDeath = step_df$flag_CohortDeath_mgm,
                                  flag_AdReg       = step_df$flag_AdReg_mgm),
                             f.update_H)
    
    # height to crown base (s1_HCB)
    step_df$s1_HCB <- pmap_dbl(list(HCB_previous     = step_df$s2_HCB,
                                    dyn_HCB          = 0,
                                    Spc_updated      = step_df$s1_Spc,
                                    flag_CohortDeath = step_df$flag_CohortDeath_mgm,
                                    flag_AdReg       = step_df$flag_AdReg_mgm),
                               f.update_HCB)
    
    # sim: calculate s1-auxiliary variables -----------------------------------
    # cell stage
    step_df$aux1_Stage <- map2_dbl(step_df$s1_H, step_df$s1_D,
                                   f.aux_stage)
    
    # basal area
    step_df$aux1_BA <- map2_dbl(step_df$s1_N, step_df$s1_D,
                                f.aux_ba)
    
    # stem volume
    step_df$aux1_Vol <- map2_dbl(step_df$s1_N, step_df$s1_D,
                                 f.aux_vol)
    
    # crown ratio
    step_df$aux1_CR <- map2_dbl(step_df$s1_H, step_df$s1_HCB,
                                f.aux_cr)
    
    # CIh, competition index for hgreg and
    # CId, competition index for dgadu
    temp_BA_cell <- step_df %>% 
      group_by(cc_ID) %>% 
      summarise(BA_cell = sum(aux1_BA)) %>% 
      pull(BA_cell)
    
    step_df$aux1_CIh <- pmap_dbl(list(Cell_ID = step_df$cc_ID,
                                      Species = step_df$s1_Spc,
                                      vec_nr1 = step_df$cc_nIDs_r1,
                                      vec_nr2 = step_df$cc_nIDs_r2),
                                 f.aux_CIh)
    step_df$aux1_CId <- pmap_dbl(list(ID      = step_df$cc_ID,
                                      Species = step_df$s1_Spc,
                                      D       = step_df$s1_D,
                                      vec_nr1 = step_df$cc_nIDs_r1,
                                      vec_nr2 = step_df$cc_nIDs_r2),
                                 f.aux_CId)
    
    # CIm, competition index for mortality of pabi
    step_df$aux1_CIm <- f.aux_CIm(step_df)
    
    # CIc, competition for hcb
    temp_IDs <-  step_df$cc_ID
    temp_D_cohorts <- step_df$s1_D
    temp_BA_cohorts <- step_df$aux1_BA
    step_df$aux1_CIc <- pmap_dbl(list(Species = step_df$s1_Spc,
                                      D       = step_df$s1_D,
                                      vec_nr1 = step_df$cc_nIDs_r1),
                                 f.aux_CIc)
    
    # sim: calculate natural forest dynamics ----------------------------------
    # temp. change (dyn_H & dyn_D)
    step_df$dyn_temp_change <- 
      case_when(step_df$aux1_Stage == 0        ~ 0.0,
                step_df$aux1_Stage %in% c(1:3) ~ pmap_dbl(
                  list(Species = step_df$s1_Spc,
                       H       = step_df$s1_H,
                       CIh     = step_df$aux1_CIh,
                       Topo    = step_df$cc_Topo),
                  f.dyn_hgreg),
                TRUE                          ~ pmap_dbl(
                  list(Species = step_df$s1_Spc,
                       D       = step_df$s1_D,
                       CId     = step_df$aux1_CId),
                  f.dyn_dgadu))
    
    # temp. state (s2_D & s2_H)
    step_df$dyn_temp_state <- 
      case_when(step_df$aux1_Stage == 0        ~ 0.0,
                step_df$aux1_Stage %in% c(1:3) ~ pmap_dbl(
                  list(Species = step_df$s1_Spc,
                       H       = step_df$s1_H,
                       dH      = step_df$dyn_temp_change),
                  f.dyn_dgreg),
                TRUE                                  ~ pmap_dbl(
                  list(Species = step_df$s1_Spc,
                       D       = step_df$s1_D,
                       dD      = step_df$dyn_temp_change),
                  f.dyn_hgadu))
    
    # height growth (dyn_H)
    step_df$dyn_H <- pmap_dbl(list(Stage       = step_df$aux1_Stage,
                                   temp_change = step_df$dyn_temp_change,
                                   temp_state  = step_df$dyn_temp_state,
                                   H           = step_df$s1_H),
                              f.dyn_hgall)
    
    # diameter growth (dyn_D)
    step_df$dyn_D <- pmap_dbl(list(Stage       = step_df$aux1_Stage,
                                   temp_change = step_df$dyn_temp_change,
                                   temp_state  = step_df$dyn_temp_state,
                                   D           = step_df$s1_D),
                              f.dyn_dgall)
    
    # ingrowth (dyn_Ningr)
    temp_H_cell <- step_df %>% 
      group_by(cc_ID) %>% 
      summarise(H_cell = sum(s1_H)) %>% 
      pull(H_cell)
    
    step_df$dyn_Ningr <- pmap_dbl(list(Cell_ID = step_df$cc_ID,
                                       Stage   = step_df$aux1_Stage,
                                       H       = step_df$s1_H,
                                       dyn_H   = step_df$dyn_H,
                                       Species = step_df$s1_Spc,
                                       CQ      = step_df$cc_CQ),
                                  f.dyn_ingr)
    
    # mortality (dyn_Nmort)
    step_df$dyn_Nmort <- pmap_dbl(list(N       = step_df$s1_N,
                                       Species = step_df$s1_Spc,
                                       D       = step_df$s1_D,
                                       dyn_D   = step_df$dyn_D,
                                       CIm     = step_df$aux1_CIm),
                                  f.dyn_mort)
    
    
    # change in height to crown base (dyn_HCB)
    step_df$dyn_HCB <- pmap_dbl(list(Species = step_df$s1_Spc,
                                     HCB     = step_df$s1_HCB,
                                     H       = step_df$s1_H,
                                     dyn_H   = step_df$dyn_H,
                                     CIc     = step_df$aux1_CIc),
                                f.dyn_hcb)
    
    # sim: determine flags ----------------------------------------------------
    # germination on empty cells (flag_Germ)
    step_df$flag_Germ <- pmap_lgl(list(Cell_ID = step_df$cc_ID,
                                       Stage   = step_df$aux1_Stage),
                                  f.flag_germ)
    
    
    # mortality flag of entire cell (flag_CohortDeath_nat)
    step_df$flag_CohortDeath_nat <- pmap_lgl(list(N     = step_df$s1_N,
                                                  dyn_N = step_df$dyn_Nmort),
                                             f.flag_cohortdeath)
    
    # advance regeneration (flag_AdReg_nat)
    step_df$flag_AdReg_nat <- map2_lgl(step_df$aux1_Stage,
                                       step_df$flag_CohortDeath_nat,
                                       f.flag_adreg)
    
    # sim: update state 2 -----------------------------------------------------
    # species
    temp_spc_selection_probs <- f.aux_spcprob(Species = step_df$s1_Spc,
                                              D       = step_df$s1_D,
                                              BA      = step_df$aux1_BA)
    
    step_df$s2_Spc <- pmap_chr(list(Spc_previous     = step_df$s1_Spc,
                                    flag_Germ        = step_df$flag_Germ,
                                    flag_CohortDeath = step_df$flag_CohortDeath_nat,
                                    flag_AdReg       = step_df$flag_AdReg_nat),
                               f.update_Spc,
                               spc_selection_probs = temp_spc_selection_probs)
    
    # stem number (s2_N)
    step_df$s2_N <- pmap_dbl(list(N_previous = step_df$s1_N,
                                  dyn_Ningr  = step_df$dyn_Ningr,
                                  dyn_Nmort  = step_df$dyn_Nmort,
                                  mgm_N      = 0),
                             f.update_N)
    
    # tree diameter (s2_D)
    step_df$s2_D <- pmap_dbl(list(D_previous       = step_df$s1_D,
                                  dyn_D            = step_df$dyn_D,
                                  flag_CohortDeath = step_df$flag_CohortDeath_nat),
                             f.update_D)
    
    # tree height (s2_H)
    step_df$s2_H <- pmap_dbl(list(H_previous       = step_df$s1_H,
                                  dyn_H            = step_df$dyn_H,
                                  flag_Germ        = step_df$flag_Germ,
                                  flag_CohortDeath = step_df$flag_CohortDeath_nat,
                                  flag_AdReg       = step_df$flag_AdReg_nat),
                             f.update_H)
    
    # height to crown base (s2_HCB)
    step_df$s2_HCB <- pmap_dbl(list(HCB_previous     = step_df$s1_HCB,
                                    dyn_HCB          = step_df$dyn_HCB,
                                    Spc_updated      = step_df$s2_Spc,
                                    flag_CohortDeath = step_df$flag_CohortDeath_nat,
                                    flag_AdReg       = step_df$flag_AdReg_nat),
                               f.update_HCB)
  }
  
  rm(i, temp_BA_cell, temp_IDs, temp_D_cohorts, temp_BA_cohorts, temp_spc_selection_probs, step_df)
  cat("\n")
  
  # res: setup --------------------------------------------------------------
  # create or empty output folder
  if (dir.exists(str_c(output_folder, param$sim_name))) {
    unlink(str_c(output_folder, param$sim_name, "/*"), recursive = TRUE)
  } else {
    dir.create(str_c(output_folder, param$sim_name), showWarnings = TRUE)
  }
  
  # res: stand development --------------------------------------------------
  cat("----STAND SUMMARY----\n")
  
  # full results (copy of step_df)
  res_full <- bind_rows(res_list)
  
  # stand development with both states (s1 and s2)
  res_standdev <- f.res_stand()
  
  # management output (s2 & mgm)
  res_mgm <- f.res_mgm_general()
  
  # res: management summary -------------------------------------------------
  if (param$mgm_type != "none") {
    cat("----MANAGEMENT SUMMARY----\n")
    
    if(param$mgm_type %in% c("RDC_tree", "RDC_cohort")) {
      res_mgm_RDC <- f.res_mgm_RDC()
    }
  }
  
  # res: outputs ------------------------------------------------------------
  cat("----SAVE DATA----\n")
  
  # save time stamp of simulation
  sink(file = str_c(output_folder, param$sim_name, "/0_execution_info.txt"))
  
  cat("-- Simulation Execution Time --", date(), "\n",
      sep = "\n")
  cat("-- System Environment Information --")
  suppressWarnings(DescTools::SysInfo())
  
  sink()
  
  # copy input files for reproducibility
  dir.create(str_c(output_folder, param$sim_name, "/1_input"), showWarnings = FALSE)
  
  input_files <- c(str_c("simsettings/", simsettings),
                   str_c("init/", param$sim_init),
                   str_c("paramfordyn/", param$sim_param))
  
  if (param$mgm_type != "none") {
    input_files <- c(input_files,
                     str_c("mgm/", param$mgm_param))
  }
  
  input_files <- str_c(input_folder, input_files)
  
  file.copy(from = input_files,
            to = str_c(output_folder, param$sim_name, "/1_input"),
            copy.date =  TRUE)
  
  # copy model code for reproducibility
  dir.create(str_c(output_folder, param$sim_name, "/2_modelcode"), showWarnings = FALSE)
  file.copy(from = list.files(path = "code/simmodel", full.names = TRUE, recursive = FALSE),
            to = str_c(output_folder, param$sim_name, "/2_modelcode"),
            recursive = FALSE, copy.date = TRUE)
  
  # save coordinates & results-dataframe
  dir.create(str_c(output_folder, param$sim_name, "/3_output"), showWarnings = FALSE)
  
  saveRDS(coordinates,
          str_c(output_folder, param$sim_name, "/3_output/coordinates.rds"))
  
  saveRDS(res_full,
          str_c(output_folder, param$sim_name, "/3_output/res_full.rds"))
  
  saveRDS(res_standdev,
          str_c(output_folder, param$sim_name, "/3_output/res_standdev.rds"))
  
  saveRDS(res_mgm,
          str_c(output_folder, param$sim_name, "/3_output/res_mgm.rds"))
  
  saveRDS(param,
          str_c(output_folder, param$sim_name, "/3_output/param.rds"))
  
  if (param$mgm_type != "none") {
    if (param$mgm_type %in% c("RDC_tree", "RDC_cohort")) {
      saveRDS(mgm_instructions,
              str_c(output_folder, param$sim_name, "/3_output/mgm_instructions_RDC.rds"))
      write_delim(res_mgm_RDC$mgm_RDC_details, delim = ";",
                  str_c(output_folder, param$sim_name, "/3_output/summary_interv_RDC.csv"))
      write_delim(res_mgm_RDC$mgm_summary_spc, delim = ";",
                  str_c(output_folder, param$sim_name, "/3_output/summary_interv_spc.csv"))
      write_delim(res_mgm_RDC$mgm_summary_tot, delim = ";",
                  str_c(output_folder, param$sim_name, "/3_output/summary_interv_tot.csv"))
    }
  }
  cat("----END----\n\n")
  
}
