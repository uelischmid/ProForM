################
##    NAIS    ##
## FUNCTIONS  ##
##    GAPS    ##
## 20.12.2021 ##
################


# hexagon metrics ---------------------------------------------------------
f.hex_param <- function(area_tot, cell_nr) {
  ## Inputs
  # "area_tot": single value, whole simulation area in m2
  # "cell_nr": single value, number of cells
  
  ## Output
  # parameters of flat topped hexagon
  # list with 4 elements, each a single value:
  #  - area: area of one hexagon in m2
  #  - size: outer radius in m
  #  - width: width (direction x) in m
  #  - height: height (direction y) in m
  
  ## Source
  # based on https://www.redblobgames.com/grids/hexagons/
  
  res <- vector(mode = "list", length = 4)
  names(res) <- c("area", "size", "width", "height")
  
  res$area <- area_tot / cell_nr
  res$size <- sqrt((2 * res$area) / (3 * sqrt(3)))
  res$width <- res$size * 2
  res$height <- res$size * sqrt(3)
  
  return(res)
}


# gap identification ------------------------------------------------------
f.gap_identify <- function(df_step_red) {
  ## Inputs
  # df_step_red: data frame with info on simulation state,
  #              reduced to the larger cohort per cell
  
  ## Output
  # gapdf: data frame with gap number per cell
  
  
  # decide whether cell is a gap or not
  gapdf <- df_step_red %>% 
    select(cc_ID, cc_nIDs_r1, Stage) %>% 
    mutate(cell_gap = case_when(Stage <= 3 ~ TRUE,
                                TRUE       ~ FALSE)) %>% 
    arrange(cc_ID)
  gap_vec <- pull(gapdf, cell_gap)
  gap_nr_current <- 1
  gap_nr_vec <- rep(NA, times = nrow(df_step_red))
  
  # loop over all cells
  for (i in 1:nrow(df_step_red)) {
    nIDs_r1_current <- unlist(gapdf[i,]$cc_nIDs_r1) # extract neighbors as vector
    
    if (gap_vec[i] == FALSE) { # if cell is no gap
      gap_nr_vec[i] <- NA
    } else { # if cell is a gap
      gap_nr_neighbors <- as.numeric(na.omit(gap_nr_vec[nIDs_r1_current])) # extract gap nr of neighboring cells as vector without NAs
      
      if (length(gap_nr_neighbors) == 0) { # if no neighboring cells are have gap number
        gap_nr_vec[i] <- gap_nr_current
        gap_nr_current <- gap_nr_current + 1
      } else { # if at least one neighboring cell has a gap number
        gap_nr_vec[i] <- min(gap_nr_neighbors) # assign smallest gap number
        
        if (length(gap_nr_neighbors) > 1) { # if there are multiple gap numbers in neighborhood
          gap_nr_vec[gap_nr_vec %in% gap_nr_neighbors] <-  min(gap_nr_neighbors) # merge all to smallest gap number
        }
      }
    }
  }
  
  # make gap numbers continuous
  unique_gap_nr_old <- sort(unique(as.numeric(na.omit(gap_nr_vec))))
  for (i in seq_along(unique_gap_nr_old)) {
    gap_nr_vec[gap_nr_vec == unique_gap_nr_old[i]] <- i
  }
  
  gapdf <- gapdf %>% 
    mutate(gap_nr = gap_nr_vec) %>% 
    filter(cell_gap == TRUE) %>% 
    select(cc_ID, gap_nr)
  
  return(gapdf)
}


f.gap_build_groups_3c <- function(c_i) {
  ## Inputs
  # c_i: cell info
  
  ## Output
  # groups: data frame with all potential groups of 3
  
  # build groups of 3
  groups <- vector(mode = "list", length = nrow(c_i) * 2)
  group_nr <- 1
  
  for (i in seq_along(c_i$cc_ID)) {
    temp_cells_neighbors <- filter(c_i, cc_ID == i)
    
    for (j in 1:2) {
      group_cells <- c(i, unlist(temp_cells_neighbors$cc_nIDs_r1)[c(j, j + 1)])
      groups[[group_nr]] <- tibble(gr_ID     = group_nr,
                                   gr_cc_IDs = group_cells)
      group_nr <- group_nr + 1
    }
  }
  groups <- bind_rows(groups)
  return(groups)
}


f.gap_identify_3c <- function(groups, df_step_red) {
  ## Inputs
  # groups:      data frame with all potential groups of 3
  # df_step_red: data frame with info on simulation state,
  #              reduced to the larger cohort per cell
  
  ## Output
  # gap_df: data frame with gap number per cell
  
  # identify cells that potentially form gaps
  cells_pot_gaps <- df_step_red %>% 
    mutate(pot_gap = case_when(Stage <= 3 ~ TRUE,
                               TRUE       ~ FALSE)) %>% 
    select(cc_ID, pot_gap)
  
  # identify groups that are part of gaps
  gap_groups <- groups %>% 
    left_join(cells_pot_gaps, by = c("gr_cc_IDs" = "cc_ID")) %>% 
    group_by(gr_ID) %>% 
    summarise(n_pot_gap_cells = sum(pot_gap)) %>% 
    mutate(gap = case_when(n_pot_gap_cells == 3 ~ TRUE,
                           TRUE                 ~ FALSE)) %>% 
    filter(gap == TRUE) %>% 
    pull(gr_ID)
  
  # identify cells that are part of gaps
  gap_cells <- groups %>% 
    filter(gr_ID %in% gap_groups) %>% 
    pull(gr_cc_IDs) %>% 
    unique()
  
  # identify and number individual gaps
  gap_df <- df_step_red %>% 
    select(cc_ID, cc_nIDs_r1) %>% 
    mutate(cell_gap = case_when(cc_ID %in% gap_cells ~ TRUE,
                                TRUE                 ~ FALSE)) %>% 
    arrange(cc_ID)
  gap_nr_current <- 1
  gap_nr_vec <- rep(NA, times = nrow(gap_df))
  
  # loop over all cells
  for (i in 1:nrow(gap_df)) {
    nIDs_r1_current <- unlist(gap_df[i,]$cc_nIDs_r1) # extract neighbors as vector
    
    # jump to next if cell is no gap
    if (gap_df[i,]$cell_gap == FALSE) next
    
    # extract gap nr of neighboring cells as vector without NAs
    gap_nr_neighbors <- as.numeric(na.omit(gap_nr_vec[nIDs_r1_current]))
    
    if (length(gap_nr_neighbors) == 0) { # if no neighboring cells are have gap number
      gap_nr_vec[i] <- gap_nr_current
      gap_nr_current <- gap_nr_current + 1
    } else { # if at least one neighboring cell has a gap number
      gap_nr_vec[i] <- min(gap_nr_neighbors) # assign smallest gap number
      
      # if there are multiple gap numbers in neighborhood, merge all to smallest gap number
      if (length(gap_nr_neighbors) > 1) { 
        gap_nr_vec[gap_nr_vec %in% gap_nr_neighbors] <-  min(gap_nr_neighbors)
      }
    }
  }
  
  # make gap numbers continuous
  unique_gap_nr_old <- sort(unique(as.numeric(na.omit(gap_nr_vec))))
  for (i in seq_along(unique_gap_nr_old)) {
    gap_nr_vec[gap_nr_vec == unique_gap_nr_old[i]] <- i
  }
  
  gap_df <- gap_df %>% 
    mutate(gap_nr = gap_nr_vec) %>% 
    filter(cell_gap == TRUE) %>% 
    select(cc_ID, gap_nr)
  
  return(gap_df)
}


# gap metrics -------------------------------------------------------------
f.gap_metrics_cell <- function(df_gaps, coor,
                               area   = TRUE,
                               length = TRUE,
                               width  = TRUE) {
  ## Input
  # df_gaps: data frame with gap-cells and gap-number
  # coor:    coordinates (dblcoor) of each cell
  # area:    should gap area be calculated?
  # length:  should gap length be calculated?
  # width:   should gap width be calculated?
  
  ## Output
  # gapmetrics:    data frame with (depending on choice)
  #                - number of cells per gap (area = TRUE)
  #                - length of gap in number of cells and hex-height (length = TRUE)
  #                - width of gap in number of cells and hex-size (width = TRUE)
  # OR
  # empty_metrics: data frame with corresponding columns and entries 0
  
  
  # error handling
  if (sum(c(area, length, width)) == 0) stop("at least one of (area, length, width) has to be TRUE")
  
  # handle empty df_gaps
  if (nrow(df_gaps) == 0) {
    empty_metrics <- tibble(gap_nr = 0)
    
    if (area == TRUE) {
      empty_metrics <- bind_cols(empty_metrics,
                                 tibble(area_n_cells = 0))
    }
    if (length == TRUE) {
      empty_metrics <- bind_cols(empty_metrics,
                                 tibble(len_max_cell   = 0,
                                        len_max_height = 0))
    }
    if (width == TRUE) {
      empty_metrics <- bind_cols(empty_metrics,
                                 tibble(wid_max_cell = 0,
                                        wid_max_size = 0))
    }
    return(empty_metrics)
  } else {
    
    gapmetrics <- tibble(gap_nr = sort(unique(df_gaps$gap_nr)))
    
    # calculate gap area
    if (area == TRUE) {
      gapmetrics_area <- df_gaps %>% 
        group_by(gap_nr) %>% 
        summarise(area_n_cells = n())
      
      gapmetrics <- gapmetrics %>% 
        left_join(gapmetrics_area, by = "gap_nr")
    }
    
    # calculate gap length
    if (length == TRUE) {
      gapdf_dblcoor_l <- df_gaps %>% 
        left_join(coor, by = c("cc_ID" = "cell_ID")) %>% 
        select(cc_ID, gap_nr, dblcoor_x, dblcoor_y) %>% 
        distinct() %>% 
        arrange(cc_ID)
      
      max_cont_gaplengths <- rep(NA, max(gapdf_dblcoor_l$gap_nr))
      
      for (gap in 1:max(gapdf_dblcoor_l$gap_nr)) {
        gapdf_current <- gapdf_dblcoor_l %>% 
          filter(gap_nr == gap)
        
        gaplengths <- rep(NA, n_distinct(gapdf_current$dblcoor_x))
        lines <- unique(gapdf_current$dblcoor_x)
        
        for (i in seq_along(lines)) {
          column_current <- gapdf_current %>% 
            filter(dblcoor_x == lines[i]) %>% 
            mutate(dist_prev = dblcoor_y - lag(dblcoor_y),
                   cumlength = c(1, rep(NA, nrow(.) - 1)))
          
          if (nrow(column_current) > 1) {
            for (a in 2:nrow(column_current)) {
              if (column_current[a, ]$dist_prev == 2) {
                column_current[a, ]$cumlength <- column_current[a - 1, ]$cumlength + 1
              } else {
                column_current[a, ]$cumlength <- 1
              }
            }
          }
          
          # check for gaps crossing simulation border
          cross_cond <- rep(FALSE, 2)
          # condition 1: last row has to be at border
          cross_cond[1] <- column_current[nrow(column_current), ]$dblcoor_y %in%
            c(max(coordinates$dblcoor_y) - 1, max(coordinates$dblcoor_y))
          # condition 2: first row has to be at border
          cross_cond[2] <- column_current[1, ]$dblcoor_y %in%
            c(min(coordinates$dblcoor_y), min(coordinates$dblcoor_y) + 1)
          
          if (sum(cross_cond) == 2) {
            # length of "first" gap
            firstgap_end <- which(column_current$dist_prev > 2)[1] - 1
            
            if (is.na(firstgap_end)) { # if the gap continues through the full gird, double the gap length
              firstgap_end <- nrow(column_current)
            }
            
            firstgap_length <- column_current[firstgap_end, ]$cumlength
            crossgap_length <- column_current[nrow(column_current), ]$cumlength + firstgap_length
          } else {
            crossgap_length <- 0
          }
          
          gaplengths[i] <- max(c(max(column_current$cumlength), crossgap_length))
          
        }
        
        max_cont_gaplengths[gap] <- max(gaplengths)
      }
      
      gapmetrics_length <- tibble(gap_nr         = sort(unique(gapdf_dblcoor_l$gap_nr)),
                                  len_max_cell   = max_cont_gaplengths,
                                  len_max_height = len_max_cell)
      
      gapmetrics <- gapmetrics %>% 
        left_join(gapmetrics_length, by = "gap_nr")
    }
    
    # calculate gap width
    if (width == TRUE) {
      gapdf_dblcoor_w <- df_gaps %>% 
        left_join(coor, by = c("cc_ID" = "cell_ID")) %>% 
        select(cc_ID, gap_nr, dblcoor_x, dblcoor_y) %>% 
        distinct() %>% 
        arrange(cc_ID)
      
      # double data frame and change dblcoor_y by plus and minus 0.5
      gapdf_dblcoor_w <- bind_rows(gapdf_dblcoor_w, gapdf_dblcoor_w) %>% 
        mutate(dblcoor_y2 = dblcoor_y + rep(c(-0.5, 0.5), each = nrow(df_gaps)))
      
      max_cont_gapwidths <- rep(NA, max(gapdf_dblcoor_w$gap_nr))
      
      for (gap in 1:max(gapdf_dblcoor_w$gap_nr)) {
        gapdf_current <- gapdf_dblcoor_w %>% 
          filter(gap_nr == gap)
        
        gapwidths <- rep(NA, n_distinct(gapdf_current$dblcoor_y2))
        lines <- sort(unique(gapdf_current$dblcoor_y2))
        
        for (i in seq_along(lines)) {
          row_current <- gapdf_current %>% 
            filter(dblcoor_y2 == lines[i]) %>% 
            arrange(dblcoor_x) %>% 
            mutate(dist_prev = dblcoor_x - lag(dblcoor_x),
                   cumwidth  = c(1, rep(NA, nrow(.) - 1)))
          
          if (nrow(row_current) > 1) {
            for (a in 2:nrow(row_current)) {
              if (row_current[a, ]$dist_prev == 1) {
                row_current[a, ]$cumwidth <- row_current[a - 1, ]$cumwidth + 1
              } else {
                row_current[a, ]$cumwidth <- 1
              }
            }
          }
          
          # check for gaps crossing simulation border
          cross_cond <- rep(FALSE, 2)
          # condition 1: last row has to be at border
          cross_cond[1] <- row_current[nrow(row_current), ]$dblcoor_x == max(coordinates$dblcoor_x)
          # condition 2: first row has to be at border
          cross_cond[2] <- row_current[1, ]$dblcoor_x == min(coordinates$dblcoor_x)
          
          if (sum(cross_cond) == 2) {
            # width of "first" gap
            firstgap_end <- which(row_current$dist_prev > 1)[1] - 1
            
            if (is.na(firstgap_end)) { # if the gap continues through the full gird, double the gap length
              firstgap_end <- nrow(row_current)
            }
            
            firstgap_width <- row_current[firstgap_end, ]$cumwidth
            crossgap_width <- row_current[nrow(row_current), ]$cumwidth + firstgap_width
          } else {
            crossgap_width <- 0
          }
          
          gapwidths[i] <- max(c(max(row_current$cumwidth), crossgap_width))
          
        }
        
        max_cont_gapwidths[gap] <- max(gapwidths)
        
      }
      
      gapmetrics_width <- tibble(gap_nr        = sort(unique(gapdf_dblcoor_w$gap_nr)),
                                 wid_max_cell  = max_cont_gapwidths) %>% 
        rowwise() %>% 
        mutate(wid_max_size = sum(rep(c(2, 1), max(coordinates$dblcoor_x))[1:wid_max_cell])) %>% 
        ungroup()
      
      gapmetrics <- gapmetrics %>% 
        left_join(gapmetrics_width, by = "gap_nr")
    }
    
    return(gapmetrics)
  }
}


f.gap_metrics_m <- function(gm_raw, hex_param, slope_deg,
                            area   = TRUE,
                            length = TRUE,
                            width  = TRUE) {
  ## Inputs:
  # gm_raw:        data frame with gap metrics in number of cells and hex-parameters
  # hex_param:     list of area, size, width, and height of hexagons
  # slope_deg:     slope of simulation area in degrees
  # area:          should gap area be calculated?
  # length:        should gap length be calculated?
  # width:         should gap width be calculated?
  
  ## Output
  # gm_m:          gap metrics in m and m2 taking slope into account
  
  
  # error handling
  if (sum(c(area, length, width)) == 0) stop("at least one of (area, length, width) has to be TRUE")
  if (area == TRUE & !("area_n_cells" %in% colnames(gm_raw))) stop("gap area not given in 'gm_raw'")
  if (length == TRUE & !("len_max_cell" %in% colnames(gm_raw))) stop("gap length not given in 'gm_raw'")
  if (width == TRUE & !("wid_max_cell" %in% colnames(gm_raw))) stop("gap width not given in 'gm_raw'")
  
  # prepare calculations
  gm_m <- select(gm_raw, gap_nr)
  slope_rad <- slope_deg * pi / 180
  dist_slope_multiplier <- 1 / cos(slope_rad)
  
  # calculate gap area
  if (area == TRUE) {
    hex_area_slope <- 6 * 0.5 * hex_param$size * (0.5 * hex_param$height * dist_slope_multiplier)
    
    gm_m <- gm_m %>% 
      mutate(area_m2 = gm_raw$area_n_cells * hex_area_slope)
  }
  
  # calculate gap length
  if (length == TRUE) {
    gm_m <- gm_m %>% 
      mutate(len_max_m     = gm_raw$len_max_height * hex_param$height * dist_slope_multiplier,
             len_max_ext_m = (gm_raw$len_max_height + 0.5) * hex_param$height * dist_slope_multiplier)
  }
  
  # calculate gap width
  if (width == TRUE) {
    gm_m <- gm_m %>% 
      mutate(wid_max_m = gm_raw$wid_max_size * hex_param$size)
  }
  
  return(gm_m)
}


# mixture -----------------------------------------------------------------
f.nais_mixture <- function(df_step, nais_pr, p = param) {
  ## Inputs
  # df_step: data frame of one timestep (based on res_standdev)
  # nais_pr: limits for basal area shares per species
  # p:       whole parameter list
  
  ## Output
  # out$stand: list with basal area shares per species
  # out$res:   list with index for each species and total mixture
  
  
  ## current species shares of basal area
  ba_sha <- df_step %>% 
    filter(D >= 12) %>% 
    group_by(Species, .drop = FALSE) %>% 
    summarise(BA_m2 = sum(BA)) %>% 
    mutate(BA_perc = BA_m2 / sum(BA_m2)) %>% 
    transpose() %>% 
    map("BA_perc")
  names(ba_sha) <- levels(df_step$Species)
  ba_sha$coni <- ba_sha$aalb + ba_sha$pabi
  
  ## compare to profile
  res <- list()
  
  # handle situation with no trees above 12 cm DBH
  if (sum(df_step$BA) == 0) {
    res$mix_aalb <- NA
    res$mix_apse <- NA
    res$mix_fsyl <- NA
    res$mix_pabi <- NA
    res$mix_coni <- NA
    res$mix <- NA
    
  } else {
    # aalb
    if (is.na(nais_pr$aalb[1])) { # aalb not defined in profile
      res$mix_aalb <- NA
    } else { # aalb defined in profile
      if (ba_sha$aalb >= nais_pr$aalb[1] & ba_sha$aalb <= nais_pr$aalb[2]) {
        res$mix_aalb <- 1
      } else if (ba_sha$aalb >= nais_pr$aalb[3] & ba_sha$aalb <= nais_pr$aalb[4]) {
        res$mix_aalb <- 0
      } else {
        res$mix_aalb <- -1
      }
    }
    
    # apse
    if (is.na(nais_pr$apse[1])) { # apse not defined in profile
      res$mix_apse <- NA
    } else { # apse defined in profile
      if (ba_sha$apse >= nais_pr$apse[1] & ba_sha$apse <= nais_pr$apse[2]) {
        res$mix_apse <- 1
      } else if (ba_sha$apse >= nais_pr$apse[3] & ba_sha$apse <= nais_pr$apse[4]) {
        res$mix_apse <- 0
      } else {
        res$mix_apse <- -1
      }
    }
    
    # fsyl
    if (is.na(nais_pr$fsyl[1])) { # fsyl not defined in profile
      res$mix_fsyl <- NA
    } else { # fsyl defined in profile
      if (ba_sha$fsyl >= nais_pr$fsyl[1] & ba_sha$fsyl <= nais_pr$fsyl[2]) {
        res$mix_fsyl <- 1
      } else if (ba_sha$fsyl >= nais_pr$fsyl[3] & ba_sha$fsyl <= nais_pr$fsyl[4]) {
        res$mix_fsyl <- 0
      } else {
        res$mix_fsyl <- -1
      }
    }
    
    # pabi
    if (is.na(nais_pr$pabi[1])) { # pabi not defined in profile
      res$mix_pabi <- NA
    } else { # pabi defined in profile
      if (ba_sha$pabi >= nais_pr$pabi[1] & ba_sha$pabi <= nais_pr$pabi[2]) {
        res$mix_pabi <- 1
      } else if (ba_sha$pabi >= nais_pr$pabi[3] & ba_sha$pabi <= nais_pr$pabi[4]) {
        res$mix_pabi <- 0
      } else {
        res$mix_pabi <- -1
      }
    }
    
    # coni
    if (is.na(nais_pr$coni[1])) { # coni not defined in profile
      res$mix_coni <- NA
    } else { # coni defined in profile
      if (ba_sha$coni >= nais_pr$coni[1] & ba_sha$coni <= nais_pr$coni[2]) {
        res$mix_coni <- 1
      } else if (ba_sha$coni >= nais_pr$coni[3] & ba_sha$coni <= nais_pr$coni[4]) {
        res$mix_coni <- 0
      } else {
        res$mix_coni <- -1
      }
    }
    
    # overall
    res$mix <- mean(c(res$mix_aalb,
                      res$mix_apse,
                      res$mix_fsyl,
                      res$mix_pabi,
                      res$mix_coni),
                    na.rm = TRUE)
  }
  
  # assure that partial values beneath 0 cannot be compensated
  if (res$mix >= 0 & sum(c(res$mix_aalb,
                           res$mix_apse,
                           res$mix_fsyl,
                           res$mix_pabi,
                           res$mix_coni) < 0, na.rm = TRUE) > 0) {
    res$mix <- -0.1
  }
  
  # combine stand and result
  out <- list()
  out$stand <- ba_sha
  names(out$stand) <- str_c("mix_", names(out$stand))
  out$res <- res
  return(out)
}


# vertical arrangement ----------------------------------------------------
f.nais_dclass <- function(df_step, nais_pr, p = param) {
  ## Inputs
  # df_step: data frame of one timestep (based on res_standdev)
  # nais_pr: limits for diameter class shares
  # p:       whole parameter list
  
  ## Output
  # out$stand: list with cohort shares per diameter class 
  # out$res:   list with index for diameter classes
  
  
  ## current shares of diameter classes
  # 0-12, 12-30, 30-50, >=50
  dclass_sha <- df_step %>% 
    mutate(cat_d = case_when(H == 0 ~ "D0",
                             D < 12 ~ "D1",
                             D < 30 ~ "D2",
                             D < 50 ~ "D3",
                             TRUE   ~ "D4")) %>% 
    filter(cat_d != "D0") %>%
    mutate(cat_d = factor(cat_d, levels = str_c("D", c(1:4)))) %>%
    count(cat_d, name = "dclass_ncoh", .drop = FALSE) %>% 
    mutate(dclass_perc = dclass_ncoh / p$sim_ncoh_tot) %>% 
    pull(dclass_perc, name = cat_d)
  
  ## compare to profile
  res <- list()
  
  if (sum(dclass_sha >= nais_pr$ideal) >= 3) {
    res$dclass <- 1
  } else if (sum(dclass_sha >= nais_pr$minimal) >= 2) {
    res$dclass <- 0
  } else {
    res$dclass <- -1
  }
  
  # combine stand and result
  out <- list()
  out$stand <- as.list(dclass_sha)
  names(out$stand) <- str_c("dclass_", names(out$stand))
  out$res <- res
  return(out)
}


f.nais_RF_nba <- function(df_step, nais_pr, p = param) {
  ## Inputs
  # df_step: step_df of current time step
  # nais_pr: list including data frame with threshold values for stem numbers and basal area
  #          with columns 'cat' (N1-N4 and BA), 'min', and 'ide', values per ha
  # p:       whole parameter list
  
  ## Output
  # out$stand: list with stem numbers per ha per diameter class
  # out$res:   list with index for stem number, and basal area
  
  
  # prepare data: current stem numbers
  sn <- df_step %>% 
    mutate(cat_d = case_when(D < 8  ~ "N0",
                             D < 12 ~ "N1",
                             D < 24 ~ "N2",
                             D < 36 ~ "N3",
                             TRUE   ~ "N4")) %>% 
    filter(cat_d != "N0") %>% 
    mutate(cat_d = factor(cat_d, levels = str_c("N", c(1:4)))) %>% 
    group_by(cat_d, .drop = FALSE) %>% 
    summarise(n_ha_cat = sum(N / p$sim_area_ha)) %>% 
    transpose() %>% 
    map("n_ha_cat")
  names(sn) <- str_c("sn_N", c(1:4))
  
  # prepare data: current basal area
  ba <- df_step %>% 
    filter(D >= 8) %>% 
    pull(BA) %>% 
    sum() / p$sim_area_ha
  
  # prepare data: convert thresholds into list
  l_thr <- nais_pr$n_ba_limits %>% 
    select(-cat) %>% 
    transpose()
  names(l_thr) <- pull(nais_pr$n_ba_limits, cat)
  
  # compare profile to stand
  res <- list()
  
  # N1, 8-12 cm DBH
  if (sn$sn_N1 >= l_thr$N1$ide) {
    res$sn_N1 <- 1
  } else if (sn$sn_N1 >= l_thr$N1$min) {
    res$sn_N1 <- 0
  } else {
    res$sn_N1 <- -1
  }
  
  # N2, 12-24 cm DBH
  if (sn$sn_N2 >= l_thr$N2$ide) {
    res$sn_N2 <- 1
  } else if (sn$sn_N2 >= l_thr$N2$min) {
    res$sn_N2 <- 0
  } else {
    res$sn_N2 <- -1
  }
  
  # N3, 24-36 cm DBH
  if (sn$sn_N3 >= l_thr$N3$ide) {
    res$sn_N3 <- 1
  } else if (sn$sn_N3 >= l_thr$N3$min) {
    res$sn_N3 <- 0
  } else {
    res$sn_N3 <- -1
  }
  
  # N4, >= 36 cm DBH
  if (sn$sn_N4 >= l_thr$N4$ide) {
    res$sn_N4 <- 1
  } else if (sn$sn_N4 >= l_thr$N4$min) {
    res$sn_N4 <- 0
  } else {
    res$sn_N4 <- -1
  }
  
  # N overall
  res$sn <- mean(c(res$sn_N1,
                   res$sn_N2,
                   res$sn_N3,
                   res$sn_N4))
  
  # BA
  if (ba >= l_thr$BA$ide) {
    res$ba_ha <- 1
  } else if (ba >= l_thr$BA$min) {
    res$ba_ha <- 0
  } else {
    res$ba_ha <- -1
  }
  
  # combine stand and result
  out <- list()
  sn$ba_ha <- ba
  out$stand <- sn
  out$res <- res
  return(out)
}


f.nais_vert <- function(indices_list) {
  ## Inputs
  # indices_list: list containing indices with at least:
  #               'dclass', 'sn_N1', 'sn_N2', 'sn_N3', 'sn_N4', 'sn', and 'ba_ha'
  
  ## Output
  # vert: value of full vertical arrangement index, checked for partial values below minimal
  
  
  # vertical arrangement overall
  vert <- mean(c(indices_list$dclass,
                 indices_list$sn,
                 indices_list$ba_ha),
               na.rm = TRUE)
  
  # assure that partial values beneath 0 cannot be compensated
  if (vert >= 0 & sum(c(indices_list$dclass,
                        indices_list$sn_N1,
                        indices_list$sn_N2,
                        indices_list$sn_N3,
                        indices_list$sn_N4,
                        indices_list$ba_ha) < 0, na.rm = TRUE) > 0) {
    vert <- -0.1
  }
  return(vert)
}


# horizontal arrangement --------------------------------------------------
f.nais_A <- function(df_step_red, gapmetrics, nais_pr, cc_red, p = param) {
  ## Inputs
  # df_step_red: data frame of current step with only larger cohort
  # gapmetrics:  gap metrics in m and m2 taking slope into account
  # nais_pr:     limits for gap length, width and canopy cover
  # cc_red:      stratum-specific canopy cover reduction factor
  # p:           whole parameter list
  
  ## Output
  # out$stand: list with max length, width and cc
  # out$res:   list with index for gap size (length and width) and canopy cover
  
  
  # error handling
  if (!("len_max_m" %in% colnames(gapmetrics))) stop("gap length in m not given in 'gapmetrics'")
  if (!("wid_max_m" %in% colnames(gapmetrics))) stop("gap width in m not given in 'gapmetrics'")
  
  # prepare data
  # number of gaps not fulfilling ideal profile
  n_gaps_not_ideal <- gapmetrics %>% 
    filter(len_max_m >= nais_pr$gaplength[1]) %>% 
    filter(wid_max_m >= nais_pr$gapwidth) %>% 
    nrow()
  
  # number of gaps not fulfilling minimal profile
  n_gaps_not_minimal <- gapmetrics %>% 
    filter(len_max_m >= nais_pr$gaplength[2]) %>% 
    filter(wid_max_m >= nais_pr$gapwidth) %>% 
    nrow()
  
  stand <- list()
  stand$n_gaps_not_ideal <- n_gaps_not_ideal
  stand$n_gaps_not_minimal <- n_gaps_not_minimal
  stand$gap_length_ext_max <- NA
  stand$gap_length_max <- NA
  stand$gap_area_max <- NA
  stand$cc <- sum(df_step_red$Stage >= 4) / nrow(df_step_red) * cc_red
  
  ## compare to profile
  res <- list()
  
  # gap-criterion (length and width)
  if (stand$n_gaps_not_ideal == 0) {
    res$gap <- 1
  } else if (stand$n_gaps_not_minimal <= 1) {
    res$gap <- 0
  } else {
    res$gap <- -1
  }
  
  # unused gap-values
  res$gap_l <- NA
  res$gap_a <- NA
  
  # canopy cover criterion
  if (stand$cc > nais_pr$cc) {
    res$cc <- 1
  } else {
    res$cc <- -1
  }
  
  # overall
  res$horiz <- mean(c(res$gap,
                      res$cc),
                    na.rm = TRUE)
  
  # assure that partial values beneath 0 cannot be compensated
  if (res$horiz >= 0 & sum(c(res$gap,
                             res$cc) < 0, na.rm = TRUE) > 0) {
    res$horiz <- -0.1
  }
  
  # combine stand and result
  out <- list()
  out$stand <- stand
  out$res <- res
  return(out)
}


f.nais_LED <- function(df_step, df_step_red, gapmetrics, gaps_cells, nais_pr_led, nais_pr_st, cc_red, p = param) {
  ## Inputs
  # df_step:     data frame of one timestep (based on res_standdev)
  # df_step_red: data frame of current step with only larger cohort
  # gapmetrics:  gap metrics in m and m2 taking slope into account
  # gaps_cells:  df linking cells to specific gaps
  # nais_pr_led: limits for gap length, width and canopy cover
  # nais_pr_st:  limits of species shares and canopy cover of saplings & thicket 
  # cc_red:      stratum-specific canopy cover reduction factor
  # p:           whole parameter list
  
  ## Output
  # out$stand: list with max area and cc
  # out$res:   list with index for gap size (area) and canopy cover
  
  
  # error handling
  if (!("area_m2" %in% colnames(gapmetrics))) stop("gap area in m2 not given in 'gapmetrics'")
  
  ## analyze gaps
  # subalpine: remove gaps with width <= 20 m from checking, never a problem
  if (p$sim_stratum == "SA") {
    gapmetrics <- gapmetrics %>% 
      filter(wid_max_m > nais_pr_led$gapwidth_SA)
  }
  
  # number of gaps not fulfilling ideal profile
  #    1: too large, irrespective of regeneration
  n_gaps_not_ideal1 <- gapmetrics %>% 
    filter(area_m2 > nais_pr_led$gaparea_suff_reg[1]) %>% 
    nrow()
  
  #    2: not enough regeneration
  pot_gaps <- gapmetrics %>% 
    filter(area_m2 > nais_pr_led$gaparea_insuff_reg[1]) %>% 
    filter(area_m2 <= nais_pr_led$gaparea_suff_reg[1]) %>% 
    pull(gap_nr)
  
  if (length(pot_gaps) > 0) {
    gaps_ideal <- vector(length = length(pot_gaps))
    
    for (a in seq_along(pot_gaps)) {
      currentgap_cellIDs <- gaps_cells %>% 
        filter(gap_nr == pot_gaps[a]) %>% 
        pull(cc_ID)
      
      step_df_gap <- df_step %>% 
        filter(cc_ID %in% currentgap_cellIDs)
      
      gaps_ideal[a] <- f.nais_sapthi(appl    = "gap_ideal",
                                     df_step = step_df_gap,
                                     nais_pr = nais_pr_st)
    }
    n_gaps_not_ideal2 <- sum(!gaps_ideal)
  } else {
    n_gaps_not_ideal2 <- 0
  }
  
  n_gaps_not_ideal <- n_gaps_not_ideal1 + n_gaps_not_ideal2
  
  # number of gaps not fulfilling minimal profile
  #    1: too large, irrespective of regeneration
  n_gaps_not_minimal1 <- gapmetrics %>% 
    filter(area_m2 > nais_pr_led$gaparea_suff_reg[2]) %>% 
    nrow()
  
  #    2: not enough regeneration
  pot_gaps <- gapmetrics %>% 
    filter(area_m2 > nais_pr_led$gaparea_insuff_reg[2]) %>% 
    filter(area_m2 <= nais_pr_led$gaparea_suff_reg[2]) %>% 
    pull(gap_nr)
  
  if (length(pot_gaps) > 0) {
    gaps_minimal <- vector(length = length(pot_gaps))
    
    for (a in seq_along(pot_gaps)) {
      currentgap_cellIDs <- gaps_cells %>% 
        filter(gap_nr == pot_gaps[a]) %>% 
        pull(cc_ID)
      
      step_df_gap <- df_step %>% 
        filter(cc_ID %in% currentgap_cellIDs)
      
      gaps_minimal[a] <- f.nais_sapthi(appl    = "gap_min",
                                       df_step = step_df_gap,
                                       nais_pr = nais_pr_st)
    }
    n_gaps_not_minimal2 <- sum(!gaps_minimal)
  } else {
    n_gaps_not_minimal2 <- 0
  }
  
  n_gaps_not_minimal <- n_gaps_not_minimal1 + n_gaps_not_minimal2
  
  ## prepare stand data
  stand <- list()
  stand$n_gaps_not_ideal <- n_gaps_not_ideal
  stand$n_gaps_not_minimal <- n_gaps_not_minimal
  stand$gap_length_ext_max <- NA
  stand$gap_length_max <- NA
  stand$gap_area_max <- NA
  stand$cc <- sum(df_step_red$Stage >= 4) / nrow(df_step_red) * cc_red
  
  ## compare to profile
  res <- list()
  
  # unused gap-values
  res$gap <- NA
  res$gap_l <- NA
  
  # gap-criterion (size and regeneration)
  if (stand$n_gaps_not_ideal == 0) {
    res$gap_a <- 1
  } else if (stand$n_gaps_not_minimal <= 1) {
    res$gap_a <- 0
  } else {
    res$gap_a <- -1
  }
  
  # canopy cover criterion
  if (stand$cc > nais_pr_led$cc[1]) {
    res$cc <- 1
  } else if (stand$cc > nais_pr_led$cc[2]) {
    res$cc <- 0
  } else {
    res$cc <- -1
  }
  
  # overall
  res$horiz <- mean(c(res$gap_a,
                      res$cc),
                    na.rm = TRUE)
  
  # assure that partial values beneath 0 cannot be compensated
  if (res$horiz >= 0 & sum(c(res$gap_a,
                             res$cc) < 0, na.rm = TRUE) > 0) {
    res$horiz <- -0.1
  }
  
  # combine stand and result
  out <- list()
  out$stand <- stand
  out$res <- res
  return(out)
}


f.nais_RF_gap <- function(gapmetrics, nais_pr) {
  ## Inputs
  # gapmetrics: gap metrics in m and m2 taking slope into account
  # nais_pr:    list including threshold value for gap length
  
  ## Output
  # out$stand: list with max length
  # out$res: list with index for gap length
  
  
  # error handling
  if (!("len_max_ext_m" %in% colnames(gapmetrics))) stop("extended gap length in m not given in 'gapmetrics'")
  
  # prepare data: maximal gap length
  stand <- list()
  stand$n_gaps_not_ideal <- NA
  stand$n_gaps_not_minimal <- NA
  stand$gap_length_ext_max <- max(gapmetrics$len_max_ext_m)
  stand$gap_length_max <- NA
  stand$gap_area_max <- NA
  stand$cc <- NA
  
  # compare profile to stand
  res <- list()
  
  # unused gap-values
  res$gap <- NA
  
  # gap length
  if (stand$gap_length_ext_max < nais_pr$gaplength_ext) {
    res$gap_l <- 1
  } else {
    res$gap_l <- -1
  }
  
  # unused gap-values and cc
  res$gap_a <- NA
  res$cc <- NA
  
  # overall
  res$horiz <- res$gap_l
  
  # combine stand and result
  out <- list()
  out$stand <- stand
  out$res <- res
  return(out)
}


f.nais_TF <- function(df_step_red, gapmetrics, nais_pr, cc_red) {
  ## Inputs
  # df_step_red: data frame of current step with only larger cohort
  # gapmetrics:  gap metrics in m and m2 taking slope into account
  # nais_pr:     list including threshold values for gap length, gap area, and canopy cover
  # cc_red:      stratum-specific canopy cover reduction factor
  
  ## Output
  # out$stand: list with gap length and area, and cc
  # out$res:   list with index for gap length, gap area, and canopy cover
  
  
  # error handling
  if (!("len_max_m" %in% colnames(gapmetrics))) stop("gap length in m not given in 'gapmetrics'")
  if (!("area_m2" %in% colnames(gapmetrics))) stop("gap area in m2 not given in 'gapmetrics'")
  
  # prepare data
  stand <- list()
  stand$n_gaps_not_ideal <- NA
  stand$n_gaps_not_minimal <- NA
  stand$gap_length_ext_max <- NA
  stand$gap_length_max <- max(gapmetrics$len_max_m)
  stand$gap_area_max <- max(gapmetrics$area_m2)
  stand$cc <- sum(df_step_red$Stage >= 4) / nrow(df_step_red) * cc_red
  
  # compare profile to stand
  res <- list()
  
  # unused gap-values
  res$gap <- NA
  
  # gap length
  if (stand$gap_length_max <= nais_pr$gaplength[1]) {
    res$gap_l <- 1
  } else if (stand$gap_length_max <= nais_pr$gaplength[2]) {
    res$gap_l <- 0
  } else {
    res$gap_l <- -1
  }
  
  # gap area
  if (stand$gap_area_max <= nais_pr$gaparea[1]) {
    res$gap_a <- 1
  } else if (stand$gap_area_max <= nais_pr$gaparea[2]) {
    res$gap_a <- 0
  } else {
    res$gap_a <- -1
  }
  
  # canopy cover criterion
  if (stand$cc >= nais_pr$cc[1]) {
    res$cc <- 1
  } else if (stand$cc >= nais_pr$cc[2]) {
    res$cc <- 0
  } else {
    res$cc <- -1
  }
  
  # overall
  res$horiz <- mean(c(res$gap_l,
                      res$gap_a,
                      res$cc),
                    na.rm = TRUE)
  
  # assure that partial values beneath 0 cannot be compensated
  if (res$horiz >= 0 & sum(c(res$gap_l,
                             res$gap_a,
                             res$cc) < 0, na.rm = TRUE) > 0) {
    res$horiz <- -0.1
  }
  
  # combine stand and result
  out <- list()
  out$stand <- stand
  out$res <- res
  return(out)
}


# support trees -----------------------------------------------------------
f.nais_cr <- function(df_step, nais_pr, p = param) {
  ## Inputs:
  # df_step: data frame of one timestep (based on res_standdev)
  # nais_pr: limits for crown ratios
  # p:       whole parameter list
  
  ## Output:
  # out$stand: list with mean crown ratios
  # out$res:   list with index for crown ratio
  
  
  # prepare calculations
  df_step2 <- df_step %>% 
    select(Species, N, D, CR) %>% 
    uncount(N) %>% 
    mutate(cat_d = case_when(D < 12 ~ "D1",
                             D < 30 ~ "D2",
                             D < 50 ~ "D3",
                             TRUE   ~ "D4"))
  
  # shares of canopy cover per diameter class
  # 0-12, 12-30, 30-50, >=50
  dclass_sha <- df_step %>% 
    mutate(cat_d = case_when(H == 0 ~ "D0",
                             D < 12 ~ "D1",
                             D < 30 ~ "D2",
                             D < 50 ~ "D3",
                             TRUE   ~ "D4")) %>% 
    filter(cat_d != "D0") %>%
    mutate(cat_d = factor(cat_d, levels = str_c("D", c(1:4)))) %>%
    count(cat_d, name = "dclass_ncoh", .drop = FALSE) %>% 
    mutate(dclass_perc = dclass_ncoh / sum(dclass_ncoh)) %>% 
    pull(dclass_perc, name = cat_d)
  
  # number of support trees per class
  dom100_N <- round(p$sim_area_ha * 100, 0)
  supptr_N <- round(dom100_N * dclass_sha, 0)
  
  # filter support trees
  temp_supptr_D1 <- df_step2 %>% 
    filter(cat_d == "D1") %>% 
    slice_max(order_by = D,
              n        = supptr_N[1],
              with_ties = FALSE)
  
  temp_supptr_D2 <- df_step2 %>% 
    filter(cat_d == "D2") %>% 
    slice_max(order_by = D,
              n        = supptr_N[2],
              with_ties = FALSE)
  
  temp_supptr_D3 <- df_step2 %>% 
    filter(cat_d == "D3") %>% 
    slice_max(order_by = D,
              n        = supptr_N[3],
              with_ties = FALSE)
  
  temp_supptr_D4 <- df_step2 %>% 
    filter(cat_d == "D4") %>% 
    slice_max(order_by = D,
              n        = supptr_N[4],
              with_ties = FALSE)
  
  supptr <- bind_rows(temp_supptr_D1,
                      temp_supptr_D2,
                      temp_supptr_D3,
                      temp_supptr_D4)
  
  # calculate mean crown ratios
  cr <- list()
  cr$cr_coni <- supptr %>% 
    filter(Species %in% c("aalb", "pabi")) %>% 
    summarise(mean_cr = mean(CR)) %>% 
    pull(mean_cr)
  cr$cr_aalb <- supptr %>% 
    filter(Species == "aalb") %>% 
    summarise(mean_cr = mean(CR)) %>% 
    pull(mean_cr)
  cr$cr_pabi <- supptr %>% 
    filter(Species == "pabi") %>% 
    summarise(mean_cr = mean(CR)) %>% 
    pull(mean_cr)
  
  # compare to profile
  res <- list()
  
  if (sum(is.na(nais_pr$coni)) == 2) { # corresponds to SM with no threshold values
    res$cr <- NA
  } else if (sum(is.na(nais_pr$coni)) == 1) { # corresponds to UM with mixed thresholds for conifers and single species
    if (is.na(cr$cr_coni)) { # no conifers among support trees
      res$cr <- NA
    } else if (cr$cr_coni >= nais_pr$coni[1]) { # conifers fulfill ideal profile
      res$cr <- 1
    } else if ((cr$cr_aalb >= nais_pr$aalb[2] & cr$cr_pabi >= nais_pr$pabi[2]) | # both species fulfill minimal profile, or ...
               (is.na(cr$cr_aalb) & cr$cr_pabi >= nais_pr$pabi[2]) | # no aalb but pabi fulfills minimal profile, or ...
               (cr$cr_aalb >= nais_pr$aalb[2] & is.na(cr$cr_pabi))) { # no pabi but aalb fulfills minimal profile
      res$cr <- 0
    } else {
      res$cr <- -1
    }
  } else { # corresponds to HM & SA with only conifer thresholds
    if (is.na(cr$cr_coni)) {
      res$cr <- NA
    } else if (cr$cr_coni >= nais_pr$coni[1]) {
      res$cr <- 1
    } else if (cr$cr_coni >= nais_pr$coni[2]) {
      res$cr <- 0
    } else {
      res$cr <- -1
    }
  }
  
  res$supptr <- res$cr
  
  # combine stand and result
  out <- list()
  out$stand <- cr
  out$res <- res
  
  return(out)
}


# seedlings ---------------------------------------------------------------
f.nais_seedl <- function(df_step, df_step_red, nais_pr, p = param) {
  ## Inputs:
  # df_step:     data frame of one timestep (based on res_standdev)
  # df_step_red: data frame of one timestep reduced to largest cohort per cell
  # nais_pr:     limits of regeneration quality and species shares
  # p:           whole parameter list
  
  # Output:
  # out$stand: list of species shares in gaps with potential regeneration
  # out$res:   list with index for seedlings
  
  
  # find gaps: CELLS without cohort < 40 cm height
  gapcells_potreg <- df_step_red %>% 
    filter(Stage <= 1) %>% 
    pull(cc_ID)
  
  # # calculate share of cells with seedlings (irrespective of species) (deprecated)
  seedl_gap_sha <- NA
  
  # check species present as seedlings
  seedl_gap_spc <- df_step %>% 
    filter(cc_ID %in% gapcells_potreg) %>%
    filter(Stage == 1) %>% 
    count(Species, .drop = FALSE) %>% 
    filter(n > 0) %>% 
    pull(Species) %>% 
    as.character()
  
  # compare to profile
  res <- list()
  
  if (length(gapcells_potreg) == 0) { # handle situation without potential regeneration cells
    res$seedl_sha <- NA
    res$seedl_spc <- NA
  } else {
    # share of gaps with seedlings (deprecated)
    res$seedl_sha <- NA
    
    # species present
    if (sum(seedl_gap_spc %in% nais_pr$species) >= nais_pr$species_n[1]) {
      res$seedl_spc <- 1
    } else if (sum(seedl_gap_spc %in% nais_pr$species) >= nais_pr$species_n[2]) {
      res$seedl_spc <- 0
    } else {
      res$seedl_spc <- -1
    }
  }
  
  # overall
  # res$seedl <- mean(c(res$seedl_sha,
  #                     res$seedl_spc),
  #                   na.rm = TRUE)
  res$seedl <- res$seedl_spc
  
  # # assure that partial values beneath 0 cannot be compensated
  # if (res$seedl >= 0 & sum(c(res$seedl_sha,
  #                            res$seedl_spc) < 0, na.rm = TRUE) > 0) {
  #   res$seedl <- -0.1
  # }
  
  # combine stand and result
  out <- list()
  out$stand$seedl_gap_sha <- seedl_gap_sha
  if (length(seedl_gap_spc) == 0) seedl_gap_spc <- "none"
  out$stand$seedl_gap_spc <- str_flatten(seedl_gap_spc, collapse = ",")
  out$res <- res
  return(out)
}


# saplings & thicket ------------------------------------------------------
f.nais_sapthi <- function(appl, df_step, nais_pr, p = param) {
  ## Inputs:
  # appl:    application case: either one of "stand", "gap_min", "gap_ideal"
  # df_step: data frame of one timestep (based on res_standdev) or reduced to a specific gap
  # nais_pr: limits of species shares and canopy cover of saplings & thicket
  # p:       whole parameter list
  
  ## Output:
  # if appl == "stand"
  #   out$stand: list with cohort shares of species within stages 2 & 3 and share of cohorts in said stages
  #   out$res:   list with indices for saplings/thicket cover and species shares
  # if appl == "gap_x"
  #   TRUE/FALSE (requirements fulfilled)
  
  
  # error handling
  if (!(appl %in% c("stand", "gap_min", "gap_ideal"))) stop("appl must be either 'stand', 'gap_min', or 'gap_ideal'")
  
  # 40 cm height to 12 cm DBH, stages 2 & 3
  df_step_st <- df_step %>% 
    filter(Stage %in% c(2, 3))
  
  # species shares within stages 2 & 3
  st_cc <- df_step_st %>% 
    count(Species, name = "ncoh_spc", .drop = FALSE) %>% 
    mutate(sha_spc = ncoh_spc / nrow(df_step_st)) %>% 
    transpose() %>% 
    map("sha_spc")
  names(st_cc) <- levels(df_step$Species)
  st_cc$coni <- st_cc$aalb + st_cc$pabi
  
  # share of CELLS with s/t
  st_cc$tot <- n_distinct(df_step_st$cc_ID) / n_distinct(df_step$cc_ID)
  
  ## compare to profile
  res <- list()
  # canopy cover of s/t
  if (appl == "gap_ideal") { # 1.5 * cc of ideal profile required
    if (st_cc$tot >= (nais_pr$tot[1] * 1.5)) {
      res$st_cc <- 1
    } else { # minimal profile not relevant in gap-case
      res$st_cc <- -1
    }
  } else {
    if (st_cc$tot >= nais_pr$tot[1]) {
      res$st_cc <- 1
    } else if (st_cc$tot >= nais_pr$tot[2]) {
      res$st_cc <- 0
    } else {
      res$st_cc <- -1
    }
  }
  
  # mixture of s/t
  if (appl == "stand") {
    # handle no s/t
    if (nrow(df_step_st) == 0) {
      res$st_mix_aalb <- NA
      res$st_mix_apse <- NA
      res$st_mix_fsyl <- NA
      res$st_mix_pabi <- NA
      res$st_mix_coni <- NA
      res$st_mix <- NA
    } else {
      # aalb
      if (is.na(nais_pr$aalb[1])) { # aalb not defined in profile
        res$st_mix_aalb <- NA
      } else { # aalb defined in profile
        if (st_cc$aalb >= nais_pr$aalb[1] & st_cc$aalb <= nais_pr$aalb[2]) {
          res$st_mix_aalb <- 1
        } else if (st_cc$aalb >= nais_pr$aalb[3] & st_cc$aalb <= nais_pr$aalb[4]) {
          res$st_mix_aalb <- 0
        } else {
          res$st_mix_aalb <- -1
        }
      }
      
      # apse
      if (is.na(nais_pr$apse[1])) { # apse not defined in profile
        res$st_mix_apse <- NA
      } else { # apse defined in profile
        if (st_cc$apse >= nais_pr$apse[1] & st_cc$apse <= nais_pr$apse[2]) {
          res$st_mix_apse <- 1
        } else if (st_cc$apse >= nais_pr$apse[3] & st_cc$apse <= nais_pr$apse[4]) {
          res$st_mix_apse <- 0
        } else {
          res$st_mix_apse <- -1
        }
      }
      
      # fsyl
      if (is.na(nais_pr$fsyl[1])) { # fsyl not defined in profile
        res$st_mix_fsyl <- NA
      } else { # fsyl defined in profile
        if (st_cc$fsyl >= nais_pr$fsyl[1] & st_cc$fsyl <= nais_pr$fsyl[2]) {
          res$st_mix_fsyl <- 1
        } else if (st_cc$fsyl >= nais_pr$fsyl[3] & st_cc$fsyl <= nais_pr$fsyl[4]) {
          res$st_mix_fsyl <- 0
        } else {
          res$st_mix_fsyl <- -1
        }
      }
      
      # pabi
      if (is.na(nais_pr$pabi[1])) { # pabi not defined in profile
        res$st_mix_pabi <- NA
      } else { # pabi defined in profile
        if (st_cc$pabi >= nais_pr$pabi[1] & st_cc$pabi <= nais_pr$pabi[2]) {
          res$st_mix_pabi <- 1
        } else if (st_cc$pabi >= nais_pr$pabi[3] & st_cc$pabi <= nais_pr$pabi[4]) {
          res$st_mix_pabi <- 0
        } else {
          res$st_mix_pabi <- -1
        }
      }
      
      # coni
      if (is.na(nais_pr$coni[1])) { # coni not defined in profile
        res$st_mix_coni <- NA
      } else { # coni defined in profile
        if (st_cc$coni >= nais_pr$coni[1] & st_cc$coni <= nais_pr$coni[2]) {
          res$st_mix_coni <- 1
        } else if (st_cc$coni >= nais_pr$coni[3] & st_cc$coni <= nais_pr$coni[4]) {
          res$st_mix_coni <- 0
        } else {
          res$st_mix_coni <- -1
        }
      }
      
      # mix overall
      res$st_mix <- mean(c(res$st_mix_aalb,
                           res$st_mix_apse,
                           res$st_mix_fsyl,
                           res$st_mix_pabi,
                           res$st_mix_coni),
                         na.rm = TRUE)
    }
  } else {
    res$st_mix_aalb <- NA
    res$st_mix_apse <- NA
    res$st_mix_fsyl <- NA
    res$st_mix_pabi <- NA
    res$st_mix_coni <- NA
    res$st_mix <- NA
  }
  
  
  # st overall
  res$sapthi <- mean(c(res$st_cc,
                       res$st_mix),
                     na.rm = TRUE)
  
  # assure that partial values beneath 0 cannot be compensated
  if (res$sapthi >= 0 & sum(c(res$st_cc,
                              res$st_mix_aalb,
                              res$st_mix_apse,
                              res$st_mix_fsyl,
                              res$st_mix_pabi,
                              res$st_mix_coni) < 0, na.rm = TRUE) > 0) {
    res$sapthi <- -0.1
  }
  
  # combine stand and result
  out <- list()
  out$stand <- st_cc
  names(out$stand) <- str_c("st_", names(out$stand))
  out$res <- res
  
  # output depending on application
  if (appl == "stand") {
    return(out)
  } else { # application to gaps
    if (res$st_cc == 1) { # ideal profile fulfilled
      return(TRUE)
    } else { # ideal profile not fulfilled
      return(FALSE)
    }
  }
}


