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


# single tree selection ---------------------------------------------------
f.mgm_single_tree_selection <- function(df_step = step_df,
                                        p       = param) {
  ## Inputs
  # df_step: current step-frame
  # p: whole parameter list
  
  ## Output
  # df_step_updated: step-frame with updated mgm_N
  
  
  # reduce df_step
  df_step_red <- select(df_step, cc_ID, co_ID, cc_nIDs_r1, cc_nIDs_r2, s2_Spc, s2_N, s2_D, aux2_BA, aux2_CR) %>% 
    mutate(aux2_BA_tree = aux2_BA / s2_N)
  
  # calculate current species shares
  BA_shares <- df_step_red %>%
    mutate(s2_Spc = factor(s2_Spc, levels = names(p$spc_shares))) %>% 
    group_by(s2_Spc, .drop = FALSE) %>% 
    summarise(BA_curr_tot = sum(aux2_BA)) %>% 
    drop_na() %>% 
    mutate(BA_curr_share   = BA_curr_tot / sum(.$BA_curr_tot),
           BA_min_share    = c(p$mgm_min_spc_sh_aalb,
                               p$mgm_min_spc_sh_apse,
                               p$mgm_min_spc_sh_fsyl,
                               p$mgm_min_spc_sh_pabi),
           harvest_spc     = case_when(BA_curr_share <= BA_min_share ~ FALSE,
                                       TRUE                          ~ TRUE),
           Pspc            = BA_curr_share / max(BA_curr_share),
           s2_Spc          = as.character(s2_Spc))
  
  spc_harvest <- BA_shares %>% 
    filter(harvest_spc == TRUE) %>% 
    pull(s2_Spc)
  
  # remove cohorts exempt from harvest
  df_mgm <- df_step_red %>% 
    filter(s2_Spc %in% spc_harvest) %>% 
    filter(s2_D >= p$mgm_minD)
  
  # stop function if no cohort is eligible for harvest
  if (nrow(df_mgm) == 0) {
    df_step_updated <- df_step %>%
      mutate(mgm_N = 0)
    return(df_step_updated)
  }
  
  # calculate selection probabilities and rank selection list
  df_mgm_ranked <- left_join(df_mgm, select(BA_shares, s2_Spc, Pspc),
                             by = "s2_Spc") %>% 
    mutate(Pba = aux2_BA_tree / max(aux2_BA_tree),
           Pcr = (1 - aux2_CR) / max(1 - aux2_CR)) %>% 
    uncount(s2_N) %>% 
    mutate(rn   = runif(nrow(.)),
           Psel = Pspc^p$mgm_w_spc * Pba^p$mgm_w_BA * Pcr^p$mgm_w_CR * rn) %>% 
    slice_sample(prop = 1) %>% 
    arrange(desc(Psel)) %>% 
    rowid_to_column(var = "rank")
  
  # harvest
  df_mgm_harvest <- df_mgm_ranked %>% 
    mutate(harvested = FALSE)
  BA_target <- sum(df_step_red$aux2_BA) * p$mgm_int
  buffer_state <- p$mgm_buffer
  harvested_cells <- NULL
  buffer_cells_r1 <- NULL
  harvested_BA <- 0
  target_BA_reached <- FALSE
  
  while (buffer_state >= 0) {
    for (i in 1:nrow(df_mgm_harvest)) {
      
      # check if tree is already harvested
      if (df_mgm_harvest[i, ]$harvested == TRUE) next
      
      # check if tree lies within buffer zone
      if (buffer_state == 2 &&
          df_mgm_harvest[i, ]$cc_ID %in%
          c(harvested_cells, buffer_cells_r1)) next
      if (buffer_state == 1 &&
          df_mgm_harvest[i, ]$cc_ID %in%
          harvested_cells) next
      
      # harvest tree
      df_mgm_harvest[i, ]$harvested <- TRUE
      harvested_cells <- c(harvested_cells, df_mgm_harvest[i, ]$cc_ID)
      buffer_cells_r1 <- unique(c(buffer_cells_r1, df_mgm_harvest[[i, "cc_nIDs_r1"]][[1]]))
      harvested_BA <- c(harvested_BA,
                        sum(tail(harvested_BA, 1), df_mgm_harvest[i, ]$aux2_BA_tree))
      
      # check target BA
      if (tail(harvested_BA, 1) > BA_target) {
        target_BA_reached <- TRUE
        break
      }
    }
    if (target_BA_reached == TRUE) break
    buffer_state <- buffer_state - 1
  }
  
  # check whether last harvested tree should be retained
  diff_last2 <- abs(tail(harvested_BA, 2) - BA_target)
  if (diff_last2[1] < diff_last2[2]) {
    df_mgm_harvest[i, ]$harvested <- FALSE
  }
  
  # calculate mgm_N and add to df_step_updated
  mgm_cohorts <- df_mgm_harvest %>% 
    filter(harvested == TRUE) %>% 
    count(co_ID, name = "mgm_N")
  
  mgm_N_vector <- select(df_step, co_ID) %>%
    left_join(mgm_cohorts, by = "co_ID") %>%
    mutate(mgm_N = replace_na(mgm_N, 0)) %>% 
    pull(mgm_N)
  
  df_step_updated <- df_step
  df_step_updated$mgm_N <- mgm_N_vector
  
  return(df_step_updated)
}



# build groups (group selection, slit cuts, cable yarding) ----------------
f.mgm_group_build_all <- function(df_step = step_df,
                                  coor    = coordinates,
                                  p       = param) {
  
  ## Inputs
  # df_step: current step-frame
  # coor: coordinates
  # p: whole parameter list
  
  ## Output
  # groups: data frame with all potential groups
  
  f.mgm_group_build_1 <- function(cc_co = cells_cohorts,
                                  cc_nb = cells_neighbors,
                                  g_cc  = group_cells,
                                  g_nr  = group_nr) {
    group_cohorts <- cc_co %>% 
      filter(cc_ID %in% g_cc) %>% 
      pull(co_ID)
    
    temp_group_cells_neighbors <- cc_nb %>% 
      filter(cc_ID %in% g_cc)
    
    temp_nr1 <- unique(unlist(c(temp_group_cells_neighbors$cc_nIDs_r1)))
    nr1 <- temp_nr1[!temp_nr1 %in% g_cc]
    
    temp_nr21 <- unique(unlist(c(temp_group_cells_neighbors$cc_nIDs_r2)))
    temp_nr22 <- temp_nr21[!temp_nr21 %in% g_cc]
    nr2 <- temp_nr22[!temp_nr22 %in% nr1]
    
    out <- tibble(gr_ID         = g_nr,
                  gr_cc_IDs     = list(g_cc),
                  gr_co_IDs     = list(group_cohorts),
                  gr_cc_nIDs_r1 = list(nr1),
                  gr_cc_nIDs_r2 = list(nr2))
    return(out)
  }
  
  cells_neighbors <- select(df_step, cc_ID, cc_nIDs_r1, cc_nIDs_r2) %>% 
    distinct() %>% 
    arrange(cc_ID)
  
  cells_cohorts <- select(df_step, cc_ID, co_ID)
  
  if (p$mgm_type == "group_selection") {
    if (p$mgm_agg == 1) {
      groups <- vector(mode = "list", length = nrow(cells_neighbors))
      group_nr <- 1
      
      for (i in seq_along(cells_neighbors$cc_ID)) {
        temp_cells_neighbors <- filter(cells_neighbors, cc_ID == i)
        group_cells <- pull(temp_cells_neighbors, cc_ID)
        groups[[group_nr]] <- f.mgm_group_build_1()
        group_nr <- group_nr + 1
      }
      groups <- bind_rows(groups)
      
    } else if (p$mgm_agg == 2) {
      groups <- vector(mode = "list", length = nrow(cells_neighbors) * 3)
      group_nr <- 1
      
      for (i in seq_along(cells_neighbors$cc_ID)) {
        temp_cells_neighbors <- filter(cells_neighbors, cc_ID == i)
        
        for (j in 1:3) {
          group_cells <- c(i, unlist(temp_cells_neighbors$cc_nIDs_r1)[j])
          groups[[group_nr]] <- f.mgm_group_build_1()
          group_nr <- group_nr + 1
        }
      }
      groups <- bind_rows(groups)
      
    } else if (p$mgm_agg == 3) {
      groups <- vector(mode = "list", length = nrow(cells_neighbors) * 2)
      group_nr <- 1
      
      for (i in seq_along(cells_neighbors$cc_ID)) {
        temp_cells_neighbors <- filter(cells_neighbors, cc_ID == i)
        
        for (j in 1:2) {
          group_cells <- c(i, unlist(temp_cells_neighbors$cc_nIDs_r1)[c(j, j + 1)])
          groups[[group_nr]] <- f.mgm_group_build_1()
          group_nr <- group_nr + 1
        }
      }
      groups <- bind_rows(groups)
      
    } else if (p$mgm_agg == 4) {
      groups <- vector(mode = "list", length = nrow(cells_neighbors) * 3)
      group_nr <- 1
      
      for (i in seq_along(cells_neighbors$cc_ID)) {
        temp_cells_neighbors <- filter(cells_neighbors, cc_ID == i)
        
        for (j in 1:3) {
          if (j < 3) {
            group_cells <- c(i, unlist(temp_cells_neighbors$cc_nIDs_r1)[c(j, j + 1, j + 2)])
          } else {
            group_cells <- c(i,
                             unlist(temp_cells_neighbors$cc_nIDs_r1)[c(2, 3)],
                             unlist(temp_cells_neighbors$cc_nIDs_r2)[4])
          }
          
          groups[[group_nr]] <- f.mgm_group_build_1()
          group_nr <- group_nr + 1
        }
      }
      groups <- bind_rows(groups)
    }
    
  } else if (p$mgm_type == "slit_cuts") {
    cells_neighbors <- cells_neighbors %>%
      mutate(R1N2 = map_dbl(cc_nIDs_r1, ~.[[2]]),
             R1N3 = map_dbl(cc_nIDs_r1, ~.[[3]]))
    
    groups <- vector(mode = "list", length = nrow(cells_neighbors))
    
    for (i in seq_along(cells_neighbors$cc_ID)) {
      group_nr <- i
      group_cells <- cells_neighbors[[i, "cc_ID"]]
      
      if (p$mgm_slit_length > 1) {
        for (l in 2:p$mgm_slit_length) {
          group_cells <- c(group_cells,
                           cells_neighbors[[group_cells[l - 1], "R1N2"]])
        }
      }
      
      if (p$mgm_slit_width > 1) {
        for (w in 2:p$mgm_slit_width) {
          prev_line <- tail(group_cells, n = p$mgm_slit_length)
          new_line <- cells_neighbors %>%
            filter(cc_ID %in% prev_line) %>%
            pull(R1N3)
          group_cells <- c(group_cells, new_line)
        }
      }
      groups[[i]] <- f.mgm_group_build_1()
    }
    groups <- bind_rows(groups)
    
  } else if (p$mgm_type == "cableyarding") {
    cells_neighbors <- cells_neighbors %>% 
      mutate(R1N1 = map_dbl(cc_nIDs_r1, ~.[[1]]))
    
    coor_red <- coor %>% 
      select(cell_ID, dblcoor_x, dblcoor_y) %>% 
      distinct()
    
    groups <- vector(mode = "list", length = (2 * p$sim_cells_nrow * length(p$mgm_cable_cols)))
    group_nr <- 1
    
    for (c in seq_along(p$mgm_cable_cols)) { # cables
      cells_cable <- coor_red %>% 
        filter(dblcoor_x == p$mgm_cable_cols[c])
      
      for (s in c("left", "right")) { # sides
        for (i in seq_along(cells_cable$dblcoor_y)) { # cable cells
          cable_cell <- cells_cable[[i, "cell_ID"]]
          
          # first cell of slit
          if (s == "left") {
            group_cells <- cells_neighbors[[cable_cell, "cc_nIDs_r1"]][[1]][[6]]
          } else {
            group_cells <- cells_neighbors[[cable_cell, "cc_nIDs_r1"]][[1]][[2]]
          }
          
          # further slit cells along length
          if (p$mgm_slit_length > 1) {
            if (s == "left") {
              neighbor_order <- rep(c(5, 6), length.out = (p$mgm_slit_length - 1))
            } else {
              neighbor_order <- rep(c(3, 2), length.out = (p$mgm_slit_length - 1))
            }
            
            for (l in seq_along(neighbor_order)) {
              group_cells <- c(group_cells,
                               cells_neighbors[[tail(group_cells, 1), "cc_nIDs_r1"]][[1]][neighbor_order[l]])
            }
          }
          
          # further slit cells along width
          if (p$mgm_slit_width > 1) {
            for (w in 2:p$mgm_slit_width) {
              prev_line <- tail(group_cells, n = p$mgm_slit_length)
              new_line <- cells_neighbors %>% 
                filter(cc_ID %in% prev_line) %>% 
                pull(R1N1)
              group_cells <- c(group_cells, new_line)
            }
          }
          
          groups[[group_nr]] <- f.mgm_group_build_1() %>% 
            mutate(cable = c,
                   side  = s)
          group_nr <- group_nr + 1
        }
      }
    }
    
    order_cells <- function(IDs, s, coor = coor_red) {
      coor %>% 
        filter(cell_ID %in% IDs) %>% 
        {if (s == "left") {
          arrange(., desc(dblcoor_x), dblcoor_y)
        } else if (s == "right") {
          arrange(., dblcoor_x, dblcoor_y)
        }} %>% 
        pull(cell_ID)
    }
    
    groups <- bind_rows(groups) %>% 
      mutate(gr_cc_IDs_ordered = map2(gr_cc_IDs, side,
                                      order_cells))
  }
  return(groups)
}



# group selection ---------------------------------------------------------
f.mgm_group_selection <- function(df_step = step_df,
                                  g_mgm   = mgm_groups,
                                  p       = param) {
  ## Inputs
  # df_step: current step-frame
  # g_mgm: data frame with group information
  # p: whole parameter list
  
  ## Output
  # df_step_updated: step-frame with updated mgm_N

   
  # reduce df_step for faster calculation
  df_step_red <- select(df_step, cc_ID, co_ID, s2_Spc, s2_D, aux2_BA, aux2_CR, aux2_Stage)
  
  # calculate current species shares
  BA_shares <- df_step_red %>%
    mutate(s2_Spc = factor(s2_Spc, levels = names(p$spc_shares))) %>% 
    group_by(s2_Spc, .drop = FALSE) %>% 
    summarise(BA_curr_tot = sum(aux2_BA)) %>% 
    drop_na() %>% 
    mutate(BA_curr_share = BA_curr_tot / sum(.$BA_curr_tot),
           BA_min_share  = c(p$mgm_min_spc_sh_aalb,
                             p$mgm_min_spc_sh_apse,
                             p$mgm_min_spc_sh_fsyl,
                             p$mgm_min_spc_sh_pabi),
           harvest_spc   = case_when(BA_curr_share <= BA_min_share ~ FALSE,
                                     TRUE                          ~ TRUE),
           Pspc          = BA_curr_share / max(BA_curr_share),
           s2_Spc        = as.character(s2_Spc))
  
  spc_no_harvest <- BA_shares %>% 
    filter(harvest_spc == FALSE) %>% 
    pull(s2_Spc)
  
  # identify cohorts exempt from harvest
  co_no_harvest_spc <- df_step_red %>% 
    filter(s2_Spc %in% spc_no_harvest) %>% 
    pull(co_ID)
  co_no_harvest_D <- df_step_red %>% 
    filter(s2_D < p$mgm_minD) %>% 
    pull(co_ID)
  co_no_harvest <- unique(c(co_no_harvest_spc, co_no_harvest_D))
  
  # stop function if no cohort is eligible for harvest
  if (length(co_no_harvest) == nrow(df_step)) {
    df_step_updated <- df_step %>%
      mutate(mgm_N = 0)
    return(df_step_updated)
  }
  
  # remove exempt cohorts and groups without cohorts eligible for harvest
  g_mgm <- g_mgm %>% 
    mutate(gr_co_IDs_red = gr_co_IDs) %>% 
    unnest(gr_co_IDs_red) %>% 
    filter(!gr_co_IDs_red %in% co_no_harvest) %>% 
    group_by(gr_ID) %>% 
    nest(gr_co_IDs_red = c(gr_co_IDs_red)) %>% 
    ungroup() %>% 
    mutate(gr_co_IDs_red = map(gr_co_IDs_red, 1))
  
  # characterize groups: Species, BA and CR
  g_mgm_co_info <- g_mgm %>% 
    select(gr_ID, gr_co_IDs_red) %>%
    unnest(gr_co_IDs_red) %>% 
    left_join(select(df_step_red, -cc_ID),
              by = c("gr_co_IDs_red" = "co_ID"))
  
  temp_gr_spc_dom <- g_mgm_co_info %>% 
    group_by(gr_ID, s2_Spc) %>% 
    summarise(BA      = sum(aux2_BA),
              .groups = "drop_last") %>% 
    slice_max(order_by = BA, n = 1, with_ties = FALSE) %>% 
    select(gr_ID, gr_Spc_dom = s2_Spc) %>% 
    ungroup()
  
  temp_gr_ba_cr <- g_mgm_co_info %>% 
    group_by(gr_ID) %>% 
    summarise(gr_BA_sum  = sum(aux2_BA),
              gr_CR_mean = weighted.mean(x = aux2_CR,
                                         w = aux2_BA))
  
  # characterize groups: regeneration within group
  df_step_red_reg_co <- df_step_red %>% 
    mutate(reg = case_when(aux2_Stage %in% c(1:3) ~ TRUE,
                           TRUE                   ~ FALSE))
  
  temp_gr_reg_group <- g_mgm %>%
    select(gr_ID, gr_co_IDs) %>% 
    unnest(gr_co_IDs) %>% 
    left_join(select(df_step_red_reg_co, -cc_ID),
              by = c("gr_co_IDs" = "co_ID")) %>% 
    group_by(gr_ID) %>% 
    summarise(gr_reg_gr_sha = sum(reg) / n())
  
  # characterize groups: regeneration among neighbors
  df_step_red_reg_cc <- df_step_red_reg_co %>% 
    group_by(cc_ID) %>% 
    summarise(n_co_reg = sum(reg)) %>% 
    mutate(cc_reg = case_when(n_co_reg == p$sim_ncoh_cell ~ TRUE,
                              TRUE                        ~ FALSE))
  
  temp_gr_reg_neighb <- g_mgm %>% 
    select(gr_ID, gr_cc_nIDs_r1) %>% 
    unnest(gr_cc_nIDs_r1) %>% 
    left_join(df_step_red_reg_cc, by = c("gr_cc_nIDs_r1" = "cc_ID")) %>% 
    group_by(gr_ID) %>% 
    summarise(gr_reg_nb_sha = sum(cc_reg) / n())
  
  # characterize groups: combine
  g_mgm <- g_mgm %>% 
    left_join(temp_gr_spc_dom,
              by = "gr_ID") %>% 
    left_join(temp_gr_ba_cr,
              by = "gr_ID") %>% 
    left_join(temp_gr_reg_group,
              by = "gr_ID") %>% 
    left_join(temp_gr_reg_neighb,
              by = "gr_ID")

  # calculate selection probabilities and rank selection list
  g_mgm_ranked <- left_join(g_mgm, select(BA_shares, s2_Spc, Pspc),
                            by = c("gr_Spc_dom" = "s2_Spc")) %>% 
    mutate(Pba  = gr_BA_sum / max(gr_BA_sum),
           Pcr  = (1 - gr_CR_mean) / max(1 - gr_CR_mean),
           Preg_gr = gr_reg_gr_sha / max(gr_reg_gr_sha),
           Preg_nb = (1 - gr_reg_nb_sha) / max(1 - gr_reg_nb_sha),
           rn   = runif(nrow(g_mgm)),
           Psel = Pspc^p$mgm_w_spc * Pba^p$mgm_w_BA * Pcr^p$mgm_w_CR * 
             Preg_gr^p$mgm_w_reg_gr * Preg_nb^p$mgm_w_reg_nb * rn) %>% 
    slice_sample(prop = 1) %>% 
    arrange(desc(Psel)) %>% 
    rowid_to_column(var = "rank")
  
  # harvest
  g_mgm_harvest <- g_mgm_ranked %>% 
    mutate(harvested = NA)
  BA_target <- sum(df_step_red$aux2_BA) * p$mgm_int
  buffer_state <- p$mgm_buffer
  harvested_groups <- NULL
  harvested_cells <- NULL
  buffer_cells_r1 <- NULL
  buffer_cells_r2 <- NULL
  harvested_BA <- 0
  target_BA_reached <- FALSE
  
  while (buffer_state >= 0) {
    for (i in 1:nrow(g_mgm_harvest)) {
      pot_harvest_cells <- g_mgm_harvest[[i, "gr_cc_IDs"]][[1]]
      
      # check if one or more cells are already harvested
      if (sum(pot_harvest_cells %in% harvested_cells) > 0) next
      
      # check if any cell lies within buffer zone
      if (buffer_state == 2 &&
          sum(pot_harvest_cells %in%
              c(buffer_cells_r1, buffer_cells_r2)) > 0) next
      if (buffer_state == 1 && 
          sum(pot_harvest_cells %in%
              buffer_cells_r1) > 0) next
      
      # harvest group
      g_mgm_harvest[i, "harvested"] <- TRUE
      harvested_groups <- c(harvested_groups, g_mgm_harvest[[i, "gr_ID"]])
      harvested_cells <- c(harvested_cells, pot_harvest_cells)
      buffer_cells_r1 <- unique(c(buffer_cells_r1, g_mgm_harvest[[i, "gr_cc_nIDs_r1"]][[1]]))
      buffer_cells_r2 <- unique(c(buffer_cells_r2, g_mgm_harvest[[i, "gr_cc_nIDs_r2"]][[1]]))
      harvested_BA <- c(harvested_BA,
                        sum(tail(harvested_BA, 1), g_mgm_harvest[i,]$gr_BA_sum, na.rm = TRUE))
      
      # check target BA
      if (tail(harvested_BA, 1) > BA_target) {
        target_BA_reached <- TRUE
        break
      } 
    }
    if (target_BA_reached == TRUE) break
    buffer_state <- buffer_state - 1
  }
  
  # check whether last harvested group should be retained
  diff_last2 <- abs(tail(harvested_BA, 2) - BA_target)
  if (diff_last2[1] < diff_last2[2]) {
    g_mgm_harvest[g_mgm_harvest$gr_ID == tail(harvested_groups, 1),]$harvested <- FALSE
  }
  
  # retrieve cohorts to be harvested and feed to df_step_updated
  harvested_cohorts <- g_mgm_harvest %>%
    filter(harvested == TRUE) %>%
    select(gr_co_IDs_red) %>%
    unnest(gr_co_IDs_red) %>%
    pull(gr_co_IDs_red)
  
  df_step_updated <- df_step %>%
    mutate(mgm_N = case_when(co_ID %in% harvested_cohorts ~ s2_N,
                             TRUE ~ 0))
  
  return(df_step_updated)
}


# cable yarding -----------------------------------------------------------
f.mgm_cy_cables <- function(coor = coordinates,
                            p    = param) {
  ## Inputs
  # coor: coordinates
  # p: whole parameter list
  
  ## Output
  # cells_cable: dataframe cell IDs per cable
  
  cells_cable <- vector(mode = "list", length = length(p$mgm_cable_cols))
  
  for (i in seq_along(p$mgm_cable_cols)) {
    cells_cable[[i]] <- coor %>% 
      select(cell_ID, dblcoor_x) %>% 
      distinct() %>% 
      filter(dblcoor_x == p$mgm_cable_cols[i]) %>% 
      mutate(cable = i) %>% 
      select(-dblcoor_x)
  }
  cells_cable <- bind_rows(cells_cable)
  
  return(cells_cable)
}


f.mgm_cableyarding <- function(df_step  = step_df,
                               g_mgm    = mgm_groups,
                               c_mgm    = mgm_cables,
                               n_interv = interv_nr,
                               p        = param) {
  ## Inputs
  # df_step: current step-frame
  # g_mgm: data frame with group information
  # c_mgm: data frame with cable cells
  # n_interv: number of current intervention
  # p: whole parameter list
  
  ## Output
  # df_step_updated: step-frame with updated mgm_N
  
  # reduce df_step for faster calculation
  df_step_red <- select(df_step, cc_ID, co_ID, s2_Spc, s2_D, aux2_BA, aux2_Stage)
  
  # calculate current species shares
  BA_shares <- df_step_red %>%
    mutate(s2_Spc = factor(s2_Spc, levels = names(p$spc_shares))) %>% 
    group_by(s2_Spc, .drop = FALSE) %>% 
    summarise(BA_curr_tot = sum(aux2_BA)) %>% 
    drop_na() %>% 
    mutate(BA_curr_share = BA_curr_tot / sum(.$BA_curr_tot),
           BA_min_share  = c(p$mgm_min_spc_sh_aalb,
                             p$mgm_min_spc_sh_apse,
                             p$mgm_min_spc_sh_fsyl,
                             p$mgm_min_spc_sh_pabi),
           harvest_spc   = case_when(BA_curr_share <= BA_min_share ~ FALSE,
                                     TRUE                          ~ TRUE),
           s2_Spc        = as.character(s2_Spc))
  
  spc_no_harvest <- BA_shares %>% 
    filter(harvest_spc == FALSE) %>% 
    pull(s2_Spc)
  
  # identify cohorts exempt from harvest in groups (all groups)
  co_no_harvest_spc <- df_step_red %>% 
    filter(s2_Spc %in% spc_no_harvest) %>% 
    pull(co_ID)
  co_no_harvest_D <- df_step_red %>% 
    filter(s2_D < p$mgm_minD) %>% 
    pull(co_ID)
  co_no_harvest <- unique(c(co_no_harvest_spc, co_no_harvest_D))
  
  # retain only groups belonging to the cable currently being harvested
  g_mgm <- g_mgm %>% 
    filter(cable == p$mgm_cable_seq[n_interv])
  
  # stop function if no cohort is eligible for harvest
  harv_co_check <- g_mgm %>% 
    select(gr_co_IDs) %>% 
    unnest(gr_co_IDs) %>% 
    unique() %>% 
    filter(!gr_co_IDs %in% co_no_harvest)
  
  if (nrow(harv_co_check) == 0) {
    df_step_updated <- df_step %>%
      mutate(mgm_N = 0)
    return(df_step_updated)
  }
  
  # remove exempt cohorts and groups without cohorts eligible for harvest
  g_mgm <- g_mgm %>% 
    mutate(gr_co_IDs_red = gr_co_IDs) %>% 
    unnest(gr_co_IDs_red) %>% 
    filter(!gr_co_IDs_red %in% co_no_harvest) %>% 
    group_by(gr_ID) %>% 
    nest(gr_co_IDs_red = c(gr_co_IDs_red)) %>% 
    ungroup() %>% 
    mutate(gr_co_IDs_red = map(gr_co_IDs_red, 1))
  
  # characterize groups: BA
  temp_gr_ba <- g_mgm %>% 
    select(gr_ID, gr_co_IDs_red) %>%
    unnest(gr_co_IDs_red) %>% 
    left_join(select(df_step_red, -cc_ID),
              by = c("gr_co_IDs_red" = "co_ID")) %>% 
    group_by(gr_ID) %>% 
    summarise(gr_BA_sum = sum(aux2_BA))
  
  # characterize groups: regeneration within group
  df_step_red_reg_co <- df_step_red %>% 
    mutate(reg = case_when(aux2_Stage %in% c(1:3) ~ TRUE,
                           TRUE                   ~ FALSE))
  
  temp_gr_reg_group <- g_mgm %>%
    select(gr_ID, gr_co_IDs) %>% 
    unnest(gr_co_IDs) %>% 
    left_join(select(df_step_red_reg_co, -cc_ID),
              by = c("gr_co_IDs" = "co_ID")) %>% 
    group_by(gr_ID) %>% 
    summarise(gr_reg_gr_sha = sum(reg) / n())
  
  # characterize groups: regeneration among neighbors
  df_step_red_reg_cc <- df_step_red_reg_co %>% 
    group_by(cc_ID) %>% 
    summarise(n_co_reg = sum(reg)) %>% 
    mutate(cc_reg = case_when(n_co_reg == p$sim_ncoh_cell ~ TRUE,
                              TRUE                        ~ FALSE))
  
  temp_gr_reg_neighb <- g_mgm %>% 
    select(gr_ID, gr_cc_nIDs_r1) %>% 
    unnest(gr_cc_nIDs_r1) %>% 
    left_join(df_step_red_reg_cc, by = c("gr_cc_nIDs_r1" = "cc_ID")) %>% 
    group_by(gr_ID) %>% 
    summarise(gr_reg_nb_sha = sum(cc_reg) / n())
  
  # characterize groups: combine
  g_mgm <- g_mgm %>% 
    left_join(temp_gr_ba,
              by = "gr_ID") %>% 
    left_join(temp_gr_reg_group,
              by = "gr_ID") %>% 
    left_join(temp_gr_reg_neighb,
              by = "gr_ID")
  
  # calculate selection probabilities and rank selection list
  g_mgm_ranked <- g_mgm %>% 
    mutate(Pba     = gr_BA_sum / max(gr_BA_sum),
           Preg_gr = gr_reg_gr_sha / max(gr_reg_gr_sha),
           Preg_nb = (1 - gr_reg_nb_sha) / max(1 - gr_reg_nb_sha),
           rn      = runif(nrow(g_mgm)),
           Psel    = Pba^p$mgm_w_BA * Preg_gr^p$mgm_w_reg_gr * Preg_nb^p$mgm_w_reg_nb * rn) %>% 
    slice_sample(prop = 1) %>% 
    arrange(desc(Psel)) %>% 
    rowid_to_column(var = "rank")
  
  # prepare harvest
  BA_target <- sum(df_step_red$aux2_BA) * p$mgm_int
  
  # harvest cable cells
  cab_cells <- c_mgm %>% 
    filter(cable == param$mgm_cable_seq[n_interv]) %>% 
    pull(cell_ID)
  cab_cohorts <- df_step_red %>% 
    filter(cc_ID %in% cab_cells) %>% 
    filter(aux2_Stage >= 3) # all cohorts with H >= 130 cm are removed
  harvested_BA <- sum(cab_cohorts$aux2_BA)
  cab_cohorts <- pull(cab_cohorts, co_ID)
  
  # harvest groups
  g_mgm_harvest <- g_mgm_ranked %>% 
    mutate(harvested = NA)
  buffer_state <- p$mgm_buffer
  g_harvested_cells <- NULL
  buffer_cells_r1 <- NULL
  buffer_cells_r2 <- NULL
  target_BA_reached <- FALSE
  
  cell_BA_all_temp <- df_step_red %>% 
    filter(!co_ID %in% co_no_harvest) %>% 
    group_by(cc_ID) %>% 
    summarise(BA_cell = sum(aux2_BA))
  
  cell_BA_all <- df_step_red %>% 
    select(cc_ID) %>% 
    distinct() %>% 
    left_join(cell_BA_all_temp, by = "cc_ID") %>% 
    replace_na(list(BA_cell = 0))
  
  while (buffer_state >= 0) {
    for (i in 1:nrow(g_mgm_harvest)) {
      pot_harvest_cells <- g_mgm_harvest[[i, "gr_cc_IDs_ordered"]][[1]]
      
      # check if one or more cells are already harvested
      if (sum(pot_harvest_cells %in% g_harvested_cells) > 0) next
      
      # check if any cell lies within buffer zone (full & partial)
      if (buffer_state == 2 &&
          sum(pot_harvest_cells %in%
              c(buffer_cells_r1, buffer_cells_r2)) > 0) next
      if (buffer_state == 1.5 &&
          sum(pot_harvest_cells %in%
              c(buffer_cells_r1, buffer_cells_r2)) >= p$mgm_slit_length) next
      if (buffer_state == 1 && 
          sum(pot_harvest_cells %in%
              buffer_cells_r1) > 0) next
      if (buffer_state == 0.5 && 
          sum(pot_harvest_cells %in%
              buffer_cells_r1) >= p$mgm_slit_length) next
      
      # harvest group
      g_mgm_harvest[i, "harvested"] <- TRUE
      buffer_cells_r1 <- unique(c(buffer_cells_r1, g_mgm_harvest[[i, "gr_cc_nIDs_r1"]][[1]]))
      buffer_cells_r2 <- unique(c(buffer_cells_r2, g_mgm_harvest[[i, "gr_cc_nIDs_r2"]][[1]]))
      
      # iteratively add cells to harvest and check target BA
      for (j in seq_along(pot_harvest_cells)) {
        cell_BA <- cell_BA_all %>% 
          filter(cc_ID == pot_harvest_cells[j]) %>% 
          pull(BA_cell)
        
        current_BA_diff <- abs(tail(harvested_BA, 1) - BA_target)
        pot_BA_diff <- abs(tail(harvested_BA, 1) + cell_BA - BA_target)
        
        if (pot_BA_diff <= current_BA_diff) {
          g_harvested_cells <- c(g_harvested_cells, pot_harvest_cells[j])
          harvested_BA <- c(harvested_BA,
                            sum(tail(harvested_BA, 1), cell_BA))
        } else {
          target_BA_reached <- TRUE
          break # breaks loop over j
        }
      }
      if (target_BA_reached == TRUE) break # breaks loop over i
    }
    if (target_BA_reached == TRUE) break # breaks while-loop
    buffer_state <- buffer_state - 0.5
  }
  
  # retrieve cohorts to be harvested and feed to df_step_updated
  g_harvested_cohorts <- df_step %>% 
    filter(cc_ID %in% g_harvested_cells) %>% 
    filter(!co_ID %in% co_no_harvest) %>% 
    pull(co_ID)
  
  harvested_cohorts <- c(cab_cohorts, g_harvested_cohorts)
  
  df_step_updated <- df_step %>%
    mutate(mgm_N = case_when(co_ID %in% harvested_cohorts ~ s2_N,
                             TRUE                         ~ 0))
  return(df_step_updated)
}
