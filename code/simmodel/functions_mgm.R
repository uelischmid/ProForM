################
##   MODEL    ##
## FUNCTIONS  ##
## MANAGEMENT ##
## 21.04.2021 ##
################



# RDC_tree: Relative Diameter Class, single tree removal ------------------
f.mgm_RDC_tree <- function(df_step      = step_df,
                           instructions = mgm_instructions) {
  ## Inputs
  # df_step: current step-frame
  # instructions: management-instructions
  
  ## Output
  # df_step_updated: step-frame with updated mgm_N
  
  
  # retrieve information of current harvest
  instructions <- filter(instructions, time_Step == unique(df_step$time_Step))
  no_classes <- max(instructions$Dclass_rel)
  species_all <- unique(instructions$Species)
  
  mgm_list_species <- vector(mode = "list", length = length(species_all))
  for (species in seq_along(species_all)) {
    min_D <- instructions %>% 
      filter(Species == species_all[species]) %>% 
      pull(Dmin) %>% 
      unique()
    
    # get list of potential harvest trees per class
    mgm_trees <- df_step %>% 
      select(cc_ID, co_ID, s2_Spc, s2_N, s2_D) %>%
      filter(s2_Spc == species_all[species]) %>%
      filter(s2_D >= min_D)
    
    if (nrow(mgm_trees) <= 1) next # jump to next species if none or one cohort is available
    
    mgm_trees <- mgm_trees %>% 
      mutate(Dclass_rel = cut_interval(s2_D, n = no_classes, labels = FALSE)) %>% 
      ungroup() %>% 
      uncount(s2_N) %>% 
      mutate(BA_tree = f.aux_ba(1, s2_D))
    
    mgm_list_class <- vector(mode = "list", length = no_classes)
    for (class in seq_along(1:no_classes)) {
      # get information and potential harvest trees for current species and class
      mgm_trees_class <- mgm_trees %>% 
        filter(Dclass_rel == class)
      if (nrow(mgm_trees_class) == 0) next # jump to next class if no trees are available for harvest
      
      BA_share_marked <- instructions %>% 
        filter(Species == species_all[species]) %>% 
        filter(Dclass_rel == class) %>% 
        pull(BA_share_marked)
      if (BA_share_marked == 0) next # jump to next class if nothing has to be harvested in this one
      
      BA_red_target <- sum(mgm_trees_class$BA_tree) * BA_share_marked
      
      # randomly order trees and approximate prescribed BA-reduction
      mgm_trees_class <- mgm_trees_class %>%
        mutate(randomTreeID = sample(c(1:nrow(mgm_trees_class)),
                                     nrow(mgm_trees_class))) %>% 
        arrange(randomTreeID) %>% 
        mutate(cumsum_BA       = cumsum(BA_tree),
               diff_target_abs = abs(cumsum_BA - BA_red_target))
      
      last_tree <- mgm_trees_class %>% 
        filter(diff_target_abs == min(diff_target_abs)) %>% 
        sample_n(1) %>% 
        pull(randomTreeID)
      
      mgm_trees_class <- mgm_trees_class %>% 
        mutate(harvest = case_when(randomTreeID <= last_tree ~ TRUE,
                                   TRUE                      ~ FALSE))
      
      # count trees to be harvested per cohort
      mgm_list_class[[class]] <- mgm_trees_class %>% 
        filter(harvest == TRUE) %>% 
        count(co_ID, name = "mgm_N")
    }
    mgm_list_species[[species]] <- bind_rows(mgm_list_class)
  }
  
  mgm_list <- bind_rows(mgm_list_species)
  
  # replace mgm_N in step_df
  mgm_N_vector <- select(df_step, co_ID) %>%
    left_join(mgm_list, by = "co_ID") %>%
    mutate(mgm_N = replace_na(mgm_N, 0)) %>% 
    pull(mgm_N)
  
  df_step_updated <- df_step
  df_step_updated$mgm_N <- mgm_N_vector
  
  return(df_step_updated)
}


# RDC_cohort: Relative Diameter Class,  whole cohort removal ---------------
f.mgm_RDC_cohort <- function(df_step      = step_df,
                             instructions = mgm_instructions) {
  ## Inputs
  # df_step: current step-frame
  # instructions: management-instructions
  
  ## Output
  # df_step_updated: step-frame with updated mgm_N
  
  
  # retrieve information of current harvest
  instructions <- filter(instructions, time_Step == unique(df_step$time_Step))
  no_classes <- max(instructions$Dclass_rel)
  species_all <- unique(instructions$Species)
  
  mgm_list_species <- vector(mode = "list", length = length(species_all))
  for (species in seq_along(species_all)) {
    min_D <- instructions %>% 
      filter(Species == species_all[species]) %>% 
      pull(Dmin) %>% 
      unique()
    
    # get list of potential harvest cohorts per class
    mgm_cohorts <- df_step %>% 
      select(cc_ID, co_ID, s2_Spc, s2_N, s2_D, aux2_BA) %>%
      filter(s2_Spc == species_all[species]) %>%
      filter(s2_D >= min_D)
    
    if (nrow(mgm_cohorts) <= 1) next # jump to next species if none or one cohort is available
    
    mgm_cohorts <- mgm_cohorts %>% 
      mutate(Dclass_rel = cut_interval(s2_D, n = no_classes, labels = FALSE))
    
    mgm_list_class <- vector(mode = "list", length = no_classes)
    for (class in seq_along(1:no_classes)) {
      # get information and potential harvest cohorts for current class
      mgm_cohorts_class <- mgm_cohorts %>% 
        filter(Dclass_rel == class)
      if (nrow(mgm_cohorts_class) == 0) next # jump to next class if no trees are available for harvest
      
      BA_share_marked <- instructions %>% 
        filter(Species == species_all[species]) %>% 
        filter(Dclass_rel == class) %>% 
        pull(BA_share_marked)
      if (BA_share_marked == 0) next # jump to next class if nothing is to be harvested in this one
      
      BA_red_target <- sum(mgm_cohorts_class$aux2_BA) * BA_share_marked
      
      # randomly order cohorts and approximate prescribed BA-reduction
      mgm_cohorts_class <- mgm_cohorts_class %>%
        mutate(randomCohortID = sample(c(1:nrow(mgm_cohorts_class)),
                                       nrow(mgm_cohorts_class))) %>% 
        arrange(randomCohortID) %>% 
        mutate(cumsum_BA       = cumsum(aux2_BA),
               diff_target_abs = abs(cumsum_BA - BA_red_target))
      
      last_cell <- mgm_cohorts_class %>% 
        filter(diff_target_abs == min(diff_target_abs)) %>% 
        sample_n(1) %>% 
        pull(randomCohortID)
      
      mgm_cohorts_class <- mgm_cohorts_class %>% 
        mutate(harvest = case_when(randomCohortID <= last_cell ~ TRUE,
                                   TRUE                        ~ FALSE))
      
      # gather cohorts to be harvested
      mgm_list_class[[class]] <- mgm_cohorts_class %>% 
        filter(harvest == TRUE) %>% 
        select(co_ID, mgm_N = s2_N)
    }
    mgm_list_species[[species]] <- bind_rows(mgm_list_class)
  }
  
  mgm_list <- bind_rows(mgm_list_species)
  
  # replace mgm_N in step_df
  mgm_N_vector <- select(df_step, co_ID) %>%
    left_join(mgm_list, by = "co_ID") %>%
    mutate(mgm_N = replace_na(mgm_N, 0)) %>% 
    pull(mgm_N)
  
  df_step_updated <- df_step
  df_step_updated$mgm_N <- mgm_N_vector
  
  return(df_step_updated)
}
