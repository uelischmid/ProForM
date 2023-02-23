####################
##      MODEL     ##
##   FUNCTIONS    ##
##    RESULTS     ##
##   31.05.2022   ##
####################


f.res_stand <- function(res = res_full) {
  ## Input
  # res: full results
  
  ## Output
  # res_standdev: stand development with both states separated (s1 and s2)
  
  res_state1 <- res %>% 
    select(cc_ID, co_ID, time_Step,
           Species = s1_Spc, N = s1_N, D = s1_D, H = s1_H, HCB = s1_HCB,
           Stage = aux1_Stage, BA = aux1_BA, Vol = aux1_Vol, CR = aux1_CR) %>% 
    filter(time_Step > 0) %>% 
    mutate(time_Step2 = time_Step,
           State_nr   = 1,
           State_name = "spring")
  
  res_state2 <- res %>% 
    select(cc_ID, co_ID, time_Step,
           Species = s2_Spc, N = s2_N, D = s2_D, H = s2_H, HCB = s2_HCB,
           Stage = aux2_Stage, BA = aux2_BA, Vol = aux2_Vol, CR = aux2_CR) %>% 
    mutate(time_Step2 = time_Step + 0.9,
           State_nr   = 2,
           State_name = "fall")
  
  res_standdev <- bind_rows(res_state1, res_state2) %>% 
    select(cc_ID, co_ID, time_Step, time_Step2, State_nr, State_name, everything()) %>% 
    mutate(State_name = factor(State_name, levels = c("spring", "fall"))) %>% 
    arrange(time_Step2, cc_ID)
  
  return(res_standdev)
}

f.res_mgm_general <- function(res = res_full) {
  ## Input
  # res: full results
  
  ## Output
  # res_mgm: management output (s2 & mgm)
  
  res_mgm <- res %>%
    select(cc_ID, co_ID, time_Step,
           s2_Spc, s2_N, s2_D, s2_H, s2_HCB,
           aux2_Stage, aux2_BA, aux2_Vol, aux2_CR,
           mgm_N) %>% 
    mutate(mgm_D        = case_when(mgm_N == 0    ~ NA_real_,
                                    TRUE          ~ s2_D),
           mgm_H        = case_when(mgm_N == 0    ~ NA_real_,
                                    TRUE          ~ s2_H),
           mgm_HCB      = case_when(mgm_N == 0    ~ NA_real_,
                                    TRUE          ~ s2_HCB),
           mgm_Stage    = case_when(mgm_N == 0    ~ NA_real_,
                                    TRUE             ~ aux2_Stage),
           mgm_BA       = case_when(mgm_N == 0    ~ NA_real_,
                                    TRUE          ~ aux2_BA / s2_N * mgm_N),
           mgm_Vol      = case_when(mgm_N == 0    ~ NA_real_,
                                    TRUE          ~ aux2_Vol / s2_N * mgm_N),
           mgm_CR       = case_when(mgm_N == 0    ~ NA_real_,
                                    TRUE         ~ aux2_CR),
           remain_N     = s2_N - mgm_N,
           remain_D     = case_when(remain_N == 0 ~ NA_real_,
                                    TRUE          ~ s2_D),
           remain_H     = case_when(remain_N == 0 ~ NA_real_,
                                    TRUE          ~ s2_H),
           remain_HCB   = case_when(remain_N == 0 ~ NA_real_,
                                    TRUE          ~ s2_HCB),
           remain_Stage = case_when(remain_N == 0 ~ NA_real_,
                                    TRUE          ~ aux2_Stage),
           remain_BA    = aux2_BA - mgm_BA,
           remain_Vol   = aux2_Vol - mgm_Vol,
           remain_CR    = case_when(remain_N == 0 ~ NA_real_,
                                    TRUE          ~ aux2_CR))
  
  return(res_mgm)
}

f.res_mgm_RDC <- function(r_mgm = res_mgm,
                          instr = mgm_instructions,
                          p     = param) {
  ## Inputs
  # r_mgm: management output as from f.res_mgm_general()
  # instr: management instructions
  # p:     full parameter-list
  
  ## Output
  # out: list with details of RDC-management
  #   out$mgm_RDC_details: mgm-details
  #   out$mgm_summary_spc: summary by species
  #   out$mgm_summary_tot: total summary
  
  # RDC-details
  mgm_RDC_details <- vector(mode = "list", length = length(p$mgm_interv_steps))
  no_classes <- max(instr$Dclass_rel)
  
  for(i in seq_along(p$mgm_interv_steps)) {
    interv_step <- p$mgm_interv_steps[i]
    
    min_D_spc <- instr %>% 
      filter(time_Step == interv_step) %>% 
      select(Species, Dmin) %>% 
      distinct()
    
    step_df_red <- r_mgm %>%
      filter(time_Step == interv_step) %>% 
      left_join(min_D_spc, by = c("s2_Spc" = "Species")) %>% 
      filter(s2_D >= Dmin) %>% 
      select(-Dmin)
    
    temp_step_df_class <- step_df_red %>%
      left_join(min_D_spc, by = c("s2_Spc" = "Species")) %>% 
      filter(s2_D >= Dmin) %>% 
      select(-Dmin) %>% 
      group_by(s2_Spc) %>% 
      filter(n() > 1) %>% 
      mutate(Dclass_rel = cut_interval(s2_D, n = no_classes, labels = FALSE)) %>%
      ungroup() %>% 
      select(co_ID, Dclass_rel)
    
    step_details <- left_join(step_df_red, temp_step_df_class, by = "co_ID") %>%
      mutate(Dclass_rel = replace_na(Dclass_rel, 0)) %>%
      group_by(time_Step, s2_Spc, Dclass_rel) %>%
      summarise(N_s2       = sum(s2_N),
                N_mgm      = sum(mgm_N),
                BA_s2_m2   = sum(aux2_BA),
                BA_mgm_m2  = sum(mgm_BA, na.rm = TRUE),
                Vol_s2_m3  = sum(aux2_Vol),
                Vol_mgm_m3 = sum(mgm_Vol, na.rm = TRUE),
                .groups    = "drop") %>%
      mutate(BA_mgm_perc   = round(100 * BA_mgm_m2 / BA_s2_m2, 2),
             Vol_mgm_perc  = round(100 * Vol_mgm_m3 / Vol_s2_m3, 2),
             N_mgm_perc    = round(100 * N_mgm / N_s2, 2)) %>%
      left_join(instr, by = c("time_Step", "s2_Spc" = "Species", "Dclass_rel")) %>%
      mutate(BA_mgm_target = round(BA_share_marked * 100, 2),
             BA_mgm_target = replace_na(BA_mgm_target, 0)) %>%
      select(time_Step, Dmin, s2_Spc,
             RDC = Dclass_rel,
             BA_s2_m2, BA_mgm_m2, BA_mgm_perc, BA_mgm_target,
             Vol_s2_m3, Vol_mgm_m3, Vol_mgm_perc,
             N_s2, N_mgm, N_mgm_perc)
    
    mgm_RDC_details[[i]] <- step_details
  }
  
  mgm_RDC_details <- bind_rows(mgm_RDC_details)
  
  # summary by species
  mgm_summary_spc <- mgm_RDC_details %>%
    group_by(time_Step, s2_Spc) %>%
    summarise(across(c(BA_s2_m2, BA_mgm_m2, Vol_s2_m3, Vol_mgm_m3, N_s2, N_mgm), sum),
              .groups = "drop") %>%
    mutate(BA_mgm_perc  = round(100 * BA_mgm_m2 / BA_s2_m2, 2),
           Vol_mgm_perc = round(100 * Vol_mgm_m3 / Vol_s2_m3, 2),
           N_mgm_perc   = round(100 * N_mgm / N_s2, 2)) %>%
    select(time_Step, Species = s2_Spc,
           BA_s2_m2, BA_mgm_m2, BA_mgm_perc,
           Vol_s2_m3, Vol_mgm_m3, Vol_mgm_perc,
           N_s2, N_mgm, N_mgm_perc)
  
  # total summary
  mgm_summary_tot <- mgm_RDC_details %>%
    group_by(time_Step) %>%
    summarise(across(c(BA_s2_m2, BA_mgm_m2, Vol_s2_m3, Vol_mgm_m3, N_s2, N_mgm), sum),
              .groups = "drop") %>%
    mutate(BA_mgm_perc  = round(100 * BA_mgm_m2 / BA_s2_m2, 2),
           Vol_mgm_perc = round(100 * Vol_mgm_m3 / Vol_s2_m3, 2),
           N_mgm_perc   = round(100 * N_mgm / N_s2, 2)) %>%
    select(time_Step,
           BA_s2_m2, BA_mgm_m2, BA_mgm_perc,
           Vol_s2_m3, Vol_mgm_m3, Vol_mgm_perc,
           N_s2, N_mgm, N_mgm_perc)
  
  # assemble output
  out <- list()
  out$mgm_RDC_details <- mgm_RDC_details
  out$mgm_summary_spc <- mgm_summary_spc
  out$mgm_summary_tot <- mgm_summary_tot
  
  return(out)
}


f.res_mgm_selection <- function(r_mgm = res_mgm,
                                p     = param) {
  ## Inputs
  # r_mgm: management output as from f.res_mgm_general()
  # p:     full parameter-list
  
  ## Output
  # out: list with details of selection-type-management
  #   out$mgm_summary_spc: summary by species
  #   out$mgm_summary_tot: total summary
  
  mgm_summary_spc <- r_mgm %>% 
    filter(time_Step %in% p$mgm_interv_steps) %>% 
    group_by(time_Step, s2_Spc) %>% 
    summarise(across(c(aux2_BA, mgm_BA, aux2_Vol, mgm_Vol, s2_N, mgm_N), sum, na.rm = TRUE),
              .groups = "drop") %>% 
    mutate(BA_mgm_perc  = round(100 * mgm_BA / aux2_BA, 2),
           Vol_mgm_perc = round(100 * mgm_Vol / aux2_Vol, 2),
           N_mgm_perc   = round(100 * mgm_N / s2_N, 2)) %>% 
    select(time_Step, Species = s2_Spc,
           BA_s2_m2 = aux2_BA, BA_mgm_m2 = mgm_BA, BA_mgm_perc,
           Vol_s2_m3 = aux2_Vol, Vol_mgm_m3 = mgm_Vol, Vol_mgm_perc,
           N_s2 = s2_N, N_mgm = mgm_N, N_mgm_perc)
  
  mgm_summary_tot <- r_mgm %>% 
    filter(time_Step %in% p$mgm_interv_steps) %>% 
    group_by(time_Step) %>% 
    summarise(across(c(aux2_BA, mgm_BA, aux2_Vol, mgm_Vol, s2_N, mgm_N), sum, na.rm = TRUE),
              .groups = "drop") %>% 
    mutate(BA_mgm_perc  = round(100 * mgm_BA / aux2_BA, 2),
           Vol_mgm_perc = round(100 * mgm_Vol / aux2_Vol, 2),
           N_mgm_perc   = round(100 * mgm_N / s2_N, 2)) %>% 
    select(time_Step,
           BA_s2_m2 = aux2_BA, BA_mgm_m2 = mgm_BA, BA_mgm_perc,
           Vol_s2_m3 = aux2_Vol, Vol_mgm_m3 = mgm_Vol, Vol_mgm_perc,
           N_s2 = s2_N, N_mgm = mgm_N, N_mgm_perc)
  
  # assemble output
  out <- list()
  out$mgm_summary_spc <- mgm_summary_spc
  out$mgm_summary_tot <- mgm_summary_tot
  
  return(out)
}
