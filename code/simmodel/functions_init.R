####################
##      MODEL     ##
##   FUNCTIONS    ##
## INITIALIZATION ##
##   07.04.2022   ##
####################



f.init_load_param_sim <- function(folder   = str_c(input_folder, "simsettings/"),
                                  filepath = simsettings) {
  ## Inputs
  # folder:   folder containing simulation settings
  # filepath: path to simsettings-file
  
  ## Output
  # param_sim: list of simulation parameters
  
  # load data
  param_sim_temp <- read_delim(str_c(folder, filepath), ";",
                               col_types = cols(
                                 parameter   = col_character(),
                                 value       = col_character(),
                                 unit        = col_character(),
                                 type        = col_character(),
                                 explanation = col_character()
                               )) %>% 
    select(name = parameter, value, type) %>% 
    rowid_to_column()
  
  param_sim <- param_sim_temp %>%
    transpose() %>%
    map("value")
  names(param_sim) <- param_sim_temp$name
  
  # change variable types
  ptype_bool <- param_sim_temp %>% 
    filter(type == "boolean") %>% 
    pull(rowid)
  for (i in ptype_bool) {
    param_sim[[i]] <- as.logical(param_sim[[i]])
  }
  
  ptype_int <- param_sim_temp %>% 
    filter(type == "integer") %>% 
    pull(rowid)
  for (i in ptype_int) {
    param_sim[[i]] <- as.double(param_sim[[i]])
  }
  
  return(param_sim)
}

f.init_load_init_data <- function(folder   = str_c(input_folder, "init/"),
                                  filepath = param_sim$sim_init) {
  ## Inputs
  # folder:   folder containing initialization data
  # filepath: path to init-data-file
  
  ## Output
  # init_data: initialization data
  
  init_data <- read_csv(str_c(folder, filepath),
                        col_types = cols())
  return(init_data)
}


f.init_load_param_fordyn <- function(folder   = str_c(input_folder, "paramfordyn/"),
                                     filepath = param_sim$sim_param,
                                     str      = param_sim$sim_stratum) {
  ## Inputs
  # folder:   folder containing parameters of forest dynamics
  # filepath: path to paramfodyn-file
  # str:      stratum
  
  ## Output
  # param_fordyn: list of forest dynamics parameters
  
  param_fordyn_temp <- read_delim(str_c(folder, filepath), ";",
                                  col_types = cols(
                                    `function` = col_character(),
                                    f.expl     = col_character(),
                                    stratum    = col_character(),
                                    species    = col_character(),
                                    p.name     = col_character(),
                                    p.value    = col_double(),
                                    p.expl     = col_character()
                                  )) %>% 
    filter(stratum == str) %>% 
    mutate(p.name2 = str_c(p.name, "_", species))
  
  param_fordyn <- param_fordyn_temp %>%
    transpose() %>% 
    map("p.value")
  names(param_fordyn) <- param_fordyn_temp$p.name2
  
  return(param_fordyn)
}


f.init_identify_spc <- function(folder   = str_c(input_folder, "paramfordyn/"),
                                filepath = param_sim$sim_param,
                                str      = param_sim$sim_stratum) {
  ## Inputs
  # folder:   folder containing parameters of forest dynamics
  # filepath: path to paramfodyn-file
  # str:      stratum
  
  ## Output
  # spc: vector of parametrized species
  
  param_fordyn_temp <- read_delim(str_c(folder, filepath), ";",
                                  col_types = cols(
                                    `function` = col_character(),
                                    f.expl     = col_character(),
                                    stratum    = col_character(),
                                    species    = col_character(),
                                    p.name     = col_character(),
                                    p.value    = col_double(),
                                    p.expl     = col_character()
                                  )) %>% 
    filter(stratum == str)
  
  spc <- param_fordyn_temp %>% 
    filter(species != "all") %>% 
    pull(species) %>% 
    unique() %>% 
    sort()
  
  return(spc)
}


f.init_load_mgm <- function(folder   = str_c(input_folder, "mgm/"),
                            filepath = param_sim$mgm_param,
                            mgm_type = param_sim$mgm_type) {
  ## Inputs
  # folder:   folder containing management data
  # filepath: path to management-file
  # mgm_type: type of management selected
  
  ## Output
  # mgm_instructions: management instructions
  
  if (!(mgm_type %in% c("none",
                        "RDC_tree", "RDC_cohort",
                        "single_tree_selection", "group_selection",
                        "slit_cuts", "cableyarding"))) stop("mgm_type unknown")
  
  if (mgm_type == "none") {
    mgm_instructions <- NA
  } else if (mgm_type %in% c("RDC_tree", "RDC_cohort")) {
    mgm_instructions <- read_csv(str_c(folder, filepath),
                                 col_types = cols(
                                   time_Step       = col_double(),
                                   Dmin            = col_double(),
                                   Species         = col_character(),
                                   Dclass_rel      = col_double(),
                                   BA_share_marked = col_double()
                                 ))
  } else {
    mgm_instructions <- read_delim(str_c(folder, filepath),
                                   delim = ";",
                                   col_types = cols_only(
                                     parameter       = col_character(),
                                     value           = col_double()
                                   ))
  }
  
  return(mgm_instructions)
}


f.init_calculate_param <- function(p_sim    = param_sim,
                                   p_fordyn = param_fordyn,
                                   dat_init = init_data,
                                   mgm_inst = mgm_instructions) {
  ## Inputs
  # p_sim:    list of simulation parameters
  # p_fordyn: list of forest dynamics parameters
  # dat_init: init data
  # mgm_inst: management instructions
  
  ## Output
  # p_sim2: updated simulation parameters list
  
  p_sim2 <- p_sim
  
  ## calculate additional parameters
  p_sim2$sim_cellsize_m2 <- p_fordyn$str_cellsize_all
  p_sim2$sim_ncells <- p_sim2$sim_cells_nrow * p_sim2$sim_cells_ncol
  p_sim2$sim_ncoh_cell <- max(dat_init$cohort_Nr)
  p_sim2$sim_ncoh_tot <- p_sim2$sim_ncells * p_sim2$sim_ncoh_cell
  p_sim2$sim_area_m2 <- p_sim2$sim_ncells * p_sim2$sim_cellsize_m2
  p_sim2$sim_area_ha <- p_sim2$sim_area_m2 / 10000
  p_sim2$spc_shares <- c(p_sim2$spc_sh_aalb, p_sim2$spc_sh_apse, p_sim2$spc_sh_fsyl, p_sim2$spc_sh_pabi)
  names(p_sim2$spc_shares) <- c("aalb", "apse", "fsyl", "pabi")
  
  # slopeaspect
  slope_rad <- p_sim2$sim_slope * pi / 180
  aspect_rad <- p_sim2$sim_aspect * pi / 180
  p_sim2$sim_sloasp <- (cos(slope_rad) + sin(slope_rad) * cos(aspect_rad)) - 1
  
  # weights for CIh by angle (only used for fsyl which only occurs in SM and UM)
  if (p_sim2$sim_stratum %in% c("SM", "UM")) {
    n_directions <- rep(c("S", "SW", "NW", "N", "NE", "SE"), 2)
    
    n_pos <- tibble(nIDs_r1_number = 1:6)
    
    if (p_sim2$sim_aspect %in% c(0:29, 330:359)) {
      n_aspects <- bind_cols(n_pos,
                             direction = n_directions[1:6])
    } else if (p_sim2$sim_aspect %in% c(30:89)) {
      n_aspects <- bind_cols(n_pos,
                             direction = n_directions[2:7])
    } else if (p_sim2$sim_aspect %in% c(90:149)) {
      n_aspects <- bind_cols(n_pos,
                             direction = n_directions[3:8])
    } else if (p_sim2$sim_aspect %in% c(150:209)) {
      n_aspects <- bind_cols(n_pos,
                             direction = n_directions[4:9])
    } else if (p_sim2$sim_aspect %in% c(210:269)) {
      n_aspects <- bind_cols(n_pos,
                             direction = n_directions[5:10])
    } else if (p_sim2$sim_aspect %in% c(270:329)) {
      n_aspects <- bind_cols(n_pos,
                             direction = n_directions[6:11])
    } 
    
    n_aspects <- n_aspects %>% 
      mutate(w1 = case_when(direction == "N"             ~ (p_fordyn$cih_wmax_fsyl - 1) * 0    + 1,
                            direction %in% c("NE", "NW") ~ (p_fordyn$cih_wmax_fsyl - 1) * 0.25 + 1,
                            direction %in% c("SE", "SW") ~ (p_fordyn$cih_wmax_fsyl - 1) * 0.75 + 1,
                            direction == "S"             ~ (p_fordyn$cih_wmax_fsyl - 1) * 1    + 1))
    p_sim2$cih_vec_weights_angle <- pull(n_aspects, w1)
  }
  
  # management parameters
  if (p_sim2$mgm_type == "none") {
    p_sim2$mgm_interv_steps <- NA
    
  } else if (p_sim2$mgm_type %in% c("RDC_tree", "RDC_cohort")) {
    p_sim2$mgm_interv_steps <- sort(unique(mgm_inst$time_Step))
    
  } else if (p_sim2$mgm_type == "single_tree_selection") {
    interv_steps <- filter(mgm_inst, parameter == "mgm_time_start") %>% pull(value)
    interv_interval <- filter(mgm_inst, parameter == "mgm_time_interval") %>% pull(value)
    while (tail(interv_steps, 1) + interv_interval <= p_sim2$sim_time) {
      interv_steps <- c(interv_steps, tail(interv_steps, 1) + interv_interval)
    }
    p_sim2$mgm_interv_steps <- interv_steps
    p_sim2$mgm_int <- filter(mgm_inst, parameter == "mgm_int") %>% pull(value)
    p_sim2$mgm_minD <- filter(mgm_inst, parameter == "mgm_minD") %>% pull(value)
    p_sim2$mgm_min_spc_sh_aalb <- filter(mgm_inst, parameter == "mgm_min_spc_sh_aalb") %>% pull(value)
    p_sim2$mgm_min_spc_sh_pabi <- filter(mgm_inst, parameter == "mgm_min_spc_sh_pabi") %>% pull(value)
    p_sim2$mgm_min_spc_sh_fsyl <- filter(mgm_inst, parameter == "mgm_min_spc_sh_fsyl") %>% pull(value)
    p_sim2$mgm_min_spc_sh_apse <- filter(mgm_inst, parameter == "mgm_min_spc_sh_apse") %>% pull(value)
    p_sim2$mgm_buffer <- filter(mgm_inst, parameter == "mgm_buffer") %>% pull(value)
    p_sim2$mgm_w_spc <- filter(mgm_inst, parameter == "mgm_w_spc") %>% pull(value)
    p_sim2$mgm_w_BA <- filter(mgm_inst, parameter == "mgm_w_BA") %>% pull(value)
    p_sim2$mgm_w_CR <- filter(mgm_inst, parameter == "mgm_w_CR") %>% pull(value)

  } else if (p_sim2$mgm_type == "group_selection") {
    interv_steps <- filter(mgm_inst, parameter == "mgm_time_start") %>% pull(value)
    interv_interval <- filter(mgm_inst, parameter == "mgm_time_interval") %>% pull(value)
    while (tail(interv_steps, 1) + interv_interval <= p_sim2$sim_time) {
      interv_steps <- c(interv_steps, tail(interv_steps, 1) + interv_interval)
    }
    p_sim2$mgm_interv_steps <- interv_steps
    p_sim2$mgm_int <- filter(mgm_inst, parameter == "mgm_int") %>% pull(value)
    p_sim2$mgm_minD <- filter(mgm_inst, parameter == "mgm_minD") %>% pull(value)
    p_sim2$mgm_min_spc_sh_aalb <- filter(mgm_inst, parameter == "mgm_min_spc_sh_aalb") %>% pull(value)
    p_sim2$mgm_min_spc_sh_pabi <- filter(mgm_inst, parameter == "mgm_min_spc_sh_pabi") %>% pull(value)
    p_sim2$mgm_min_spc_sh_fsyl <- filter(mgm_inst, parameter == "mgm_min_spc_sh_fsyl") %>% pull(value)
    p_sim2$mgm_min_spc_sh_apse <- filter(mgm_inst, parameter == "mgm_min_spc_sh_apse") %>% pull(value)
    p_sim2$mgm_agg <- filter(mgm_inst, parameter == "mgm_agg") %>% pull(value)
    p_sim2$mgm_buffer <- filter(mgm_inst, parameter == "mgm_buffer") %>% pull(value)
    p_sim2$mgm_w_spc <- filter(mgm_inst, parameter == "mgm_w_spc") %>% pull(value)
    p_sim2$mgm_w_BA <- filter(mgm_inst, parameter == "mgm_w_BA") %>% pull(value)
    p_sim2$mgm_w_CR <- filter(mgm_inst, parameter == "mgm_w_CR") %>% pull(value)
    p_sim2$mgm_w_reg_gr <- filter(mgm_inst, parameter == "mgm_w_reg_gr") %>% pull(value)
    p_sim2$mgm_w_reg_nb <- filter(mgm_inst, parameter == "mgm_w_reg_nb") %>% pull(value)
    
  } else if (p_sim2$mgm_type == "slit_cuts") {
    interv_steps <- filter(mgm_inst, parameter == "mgm_time_start") %>% pull(value)
    interv_interval <- filter(mgm_inst, parameter == "mgm_time_interval") %>% pull(value)
    while (tail(interv_steps, 1) + interv_interval <= p_sim2$sim_time) {
      interv_steps <- c(interv_steps, tail(interv_steps, 1) + interv_interval)
    }
    p_sim2$mgm_interv_steps <- interv_steps
    p_sim2$mgm_int <- filter(mgm_inst, parameter == "mgm_int") %>% pull(value)
    p_sim2$mgm_minD <- filter(mgm_inst, parameter == "mgm_minD") %>% pull(value)
    p_sim2$mgm_min_spc_sh_aalb <- filter(mgm_inst, parameter == "mgm_min_spc_sh_aalb") %>% pull(value)
    p_sim2$mgm_min_spc_sh_pabi <- filter(mgm_inst, parameter == "mgm_min_spc_sh_pabi") %>% pull(value)
    p_sim2$mgm_min_spc_sh_fsyl <- filter(mgm_inst, parameter == "mgm_min_spc_sh_fsyl") %>% pull(value)
    p_sim2$mgm_min_spc_sh_apse <- filter(mgm_inst, parameter == "mgm_min_spc_sh_apse") %>% pull(value)
    p_sim2$mgm_slit_length <- filter(mgm_inst, parameter == "mgm_slit_length") %>% pull(value)
    p_sim2$mgm_slit_width <- filter(mgm_inst, parameter == "mgm_slit_width") %>% pull(value)
    p_sim2$mgm_buffer <- filter(mgm_inst, parameter == "mgm_buffer") %>% pull(value)
    p_sim2$mgm_w_spc <- filter(mgm_inst, parameter == "mgm_w_spc") %>% pull(value)
    p_sim2$mgm_w_BA <- filter(mgm_inst, parameter == "mgm_w_BA") %>% pull(value)
    p_sim2$mgm_w_CR <- filter(mgm_inst, parameter == "mgm_w_CR") %>% pull(value)
    p_sim2$mgm_w_reg_gr <- filter(mgm_inst, parameter == "mgm_w_reg_gr") %>% pull(value)
    p_sim2$mgm_w_reg_nb <- filter(mgm_inst, parameter == "mgm_w_reg_nb") %>% pull(value)
    
  } else if (p_sim2$mgm_type == "cableyarding") {
    interv_steps <- filter(mgm_inst, parameter == "mgm_time_start") %>% pull(value)
    interv_interval <- filter(mgm_inst, parameter == "mgm_time_interval") %>% pull(value)
    while (tail(interv_steps, 1) + interv_interval <= p_sim2$sim_time) {
      interv_steps <- c(interv_steps, tail(interv_steps, 1) + interv_interval)
    }
    p_sim2$mgm_interv_steps <- interv_steps
    p_sim2$mgm_int <- filter(mgm_inst, parameter == "mgm_int") %>% pull(value)
    p_sim2$mgm_minD <- filter(mgm_inst, parameter == "mgm_minD") %>% pull(value)
    p_sim2$mgm_min_spc_sh_aalb <- filter(mgm_inst, parameter == "mgm_min_spc_sh_aalb") %>% pull(value)
    p_sim2$mgm_min_spc_sh_pabi <- filter(mgm_inst, parameter == "mgm_min_spc_sh_pabi") %>% pull(value)
    p_sim2$mgm_min_spc_sh_fsyl <- filter(mgm_inst, parameter == "mgm_min_spc_sh_fsyl") %>% pull(value)
    p_sim2$mgm_min_spc_sh_apse <- filter(mgm_inst, parameter == "mgm_min_spc_sh_apse") %>% pull(value)
    mgm_cable_cols <- c(filter(mgm_inst, parameter == "mgm_cable_col_1") %>% pull(value),
                        filter(mgm_inst, parameter == "mgm_cable_col_2") %>% pull(value),
                        filter(mgm_inst, parameter == "mgm_cable_col_3") %>% pull(value))
    p_sim2$mgm_cable_cols <- mgm_cable_cols[!is.na(mgm_cable_cols)]
    p_sim2$mgm_cable_seq <- rep(seq_along(p_sim2$mgm_cable_cols),
                                length.out = length(p_sim2$mgm_interv_steps))
    p_sim2$mgm_slit_length <- filter(mgm_inst, parameter == "mgm_slit_length") %>% pull(value)
    p_sim2$mgm_slit_width <- filter(mgm_inst, parameter == "mgm_slit_width") %>% pull(value)
    p_sim2$mgm_buffer <- filter(mgm_inst, parameter == "mgm_buffer") %>% pull(value)
    p_sim2$mgm_w_BA <- filter(mgm_inst, parameter == "mgm_w_BA") %>% pull(value)
    p_sim2$mgm_w_reg_gr <- filter(mgm_inst, parameter == "mgm_w_reg_gr") %>% pull(value)
    p_sim2$mgm_w_reg_nb <- filter(mgm_inst, parameter == "mgm_w_reg_nb") %>% pull(value)
  }
  
  # prefactors
  f.pf <- function(min, max, qual) { # helper function
    ## Inputs
    # min:  prefactor value at quality = 1
    # max:  prefactor value at quality = 5
    # qual: site or regeneration quality (1-5)
    
    ## Output
    # pf: prefactor
    
    pf <- min + ((max - min) / 4) * (qual - 1)
    return(pf)
  }
  
  # regeneration quality: germination
  p_sim2$pf_gi <- f.pf(min = p_fordyn$pf_gi_min_all,
                       max = 1,
                       qual = p_sim2$sim_qual_reg)
  
  # regeneration quality: height growth of regeneration
  p_sim2$pf_dh <- f.pf(min = p_fordyn$pf_dh_min_all,
                       max = 1,
                       qual = p_sim2$sim_qual_reg)
  
  ## site quality
  p_sim2$pf_dd <- f.pf(min = p_fordyn$pf_dd_min_all,
                       max = 1,
                       qual = p_sim2$sim_qual_site)
  
  
  return(p_sim2)
}


f.init_check_data <- function(p        = param,
                              dat_init = init_data,
                              spc      = species_parametrized) {
  ## Inputs
  # param:    parameter list
  # dat_init: initialization data
  # spc:      vector of parametrized species
  
  # Output
  # check: TRUE if all is ok, otherwise function is aborted
  
  error_msg <- c("\n\n----------- ERROR-----------\n\n",
                 "\n\n-----------------------------\n(hit escape to stop execution)\n-----------------------------\n\n")
  
  # sim-settings: number of columns of simulation grid has to be even
  error_text <- "number of columns of simulation grid\nhas to be even, otherwise neighbors\ncannot be calculated correctly"
  if (p$sim_cells_ncol %% 2 != 0) {
    cat(str_c(error_msg[1], error_text, error_msg[2]))
    quit(save = "ask")
  }
  
  # sim-settings: aspect has to be an integer
  error_text <- "aspect of simulation site\nhas to be an integer, otherwise neighbor-weights\ncannot be calculated correctly"
  if (p$sim_aspect %% 1 != 0) {
    cat(str_c(error_msg[1], error_text, error_msg[2]))
    quit(save = "ask")
  }
  
  # sim-settings: site and regeneration quality have to be integers from 1-5
  error_text <- "site and regeneration quality\nhave to be an integers between 1-5"
  if (p$sim_qual_site %% 1 != 0 | p$sim_qual_site < 1 | p$sim_qual_site > 5) {
    cat(str_c(error_msg[1], error_text, error_msg[2]))
    quit(save = "ask")
  }
  if (p$sim_qual_reg %% 1 != 0 | p$sim_qual_reg < 1 | p$sim_qual_reg > 5) {
    cat(str_c(error_msg[1], error_text, error_msg[2]))
    quit(save = "ask")
  }
  
  # sim-settings: species-share have to sum to 100
  error_text <- "species shares have to sum to 100"
  if (sum(p$spc_shares) != 100) {
    cat(str_c(error_msg[1], error_text, error_msg[2]))
    quit(save = "ask")
  }
  
  # sim-settings: species-pressure has to be between 0 and 100
  error_text <- "species pressure has to be between 0 and 100"
  if (p$spc_pressure < 0 || p$spc_pressure > 100) {
    cat(str_c(error_msg[1], error_text, error_msg[2]))
    quit(save = "ask")
  }
  
  # sim-settings and init-data: number of cells must match dimensions provided in sim-settings
  error_text <- "number of cohorts in init-file\nand dimensions specified in\nsim-settings do not match"
  if (nrow(dat_init) != p$sim_ncoh_tot) {
    cat(str_c(error_msg[1], error_text, error_msg[2]))
    quit(save = "ask")
  }
  
  # sim-settings and parameters: species with a share > 0 have to be parameterized
  error_text <- "not all species with an assigned\nshare in the sim-settings are\nparameterized"
  if (sum(names(p$spc_shares[p$spc_shares > 0]) %in% spc) !=
      length(p$spc_shares[p$spc_shares > 0])) {
    cat(str_c(error_msg[1], error_text, error_msg[2]))
    quit(save = "ask")
  }
  
  # init-data: must be complete
  error_text <- "init-file has missing values"
  if (sum(is.na(dat_init)) > 0) {
    cat(str_c(error_msg[1], error_text, error_msg[2]))
    quit(save = "ask")
  }
  
  # init-data: Species must be either "aalb", "apse", "fsyl", "pabi", or "none"
  correct_values <- c(names(p$spc_shares), "none")
  error_text <- str_c("invalid entry in Species\nmust be either\n", str_c(names(p$spc_shares), collapse = ", "), ", or none")
  test <- unique(dat_init$Species)
  for (i in seq_along(test)) {
    if (!(test[i] %in% correct_values)) {
      cat(str_c(error_msg[1], error_text, error_msg[2]))
      quit(save = "ask")
    }
  }
  
  check <- TRUE
  return(check)
}


f.init_grid <- function(dat_init = init_data,
                        p        = param) {
  ## Inputs
  # dat_init: initialization data
  # p:        whole parameter list
  
  ## Output
  # coordinates: coordinates of cell centers and neighbors,
  #              in doubled and cartesian coordinates
  
  # calculate hexagon metrics
  flat_hex <- list()
  flat_hex$area <- p$sim_area_m / p$sim_ncells
  flat_hex$size <- sqrt((2 * flat_hex$area) / (3 * sqrt(3)))
  flat_hex$width <- flat_hex$size * 2
  flat_hex$height <- flat_hex$size * sqrt(3)
  
  # calculate center coordinates
  coordinates_centers <- dat_init %>% 
    select(cell_ID) %>% 
    distinct() %>% 
    mutate(aux_col    = rep(1:p$sim_cells_ncol, each = p$sim_cells_nrow),
           aux_row    = rep(1:p$sim_cells_nrow, times = p$sim_cells_ncol),
           dblcoor_x  = aux_col,
           dblcoor_y = case_when(aux_col %% 2 == 0 ~ aux_row * 2,
                                 TRUE               ~ aux_row * 2 - 1),
           cartcoor_x = dblcoor_x * 0.75 * flat_hex$width,
           cartcoor_y = dblcoor_y * 0.5 * flat_hex$height) %>% 
    select(-c(aux_col, aux_row))
  
  # calculate corner coordinates
  f.hex_coor_corner <- function(center_x, center_y, hex_size) {
    res <- tibble(corner_nr = 1:6) %>% 
      mutate(angle_rad = pi / 180 * (60 * corner_nr),
             corner_x = center_x + hex_size * cos(angle_rad),
             corner_y = center_y + hex_size * sin(angle_rad)) %>% 
      select(-angle_rad)
    return(res)
  }
  
  coordinates <- coordinates_centers %>% 
    mutate(corners = map2(cartcoor_x, cartcoor_y,
                          f.hex_coor_corner,
                          hex_size = flat_hex$size)) %>% 
    unnest(corners)
  
  return(coordinates)
}


f.init_neighbors <- function(coor = coordinates) {
  ## Inputs
  # coor:   coordinates of cell centers and neighbors
  
  ## Output
  # neighbor_ids_vec: df with listcolumns holding IDs of neighbors for each cell
  
  # extend map to find neighbors around "edges"
  nmax_x <- max(coor$dblcoor_x)
  nmax_y <- max(coor$dblcoor_y)
  
  coor_extended <- list()
  coor_extended$orig <- coor %>% 
    select(cell_ID, dblcoor_x, dblcoor_y) %>% 
    distinct()
  coor_extended$tl <- coor_extended$orig %>% 
    mutate(dblcoor_x = dblcoor_x - nmax_x,
           dblcoor_y = dblcoor_y + nmax_y)
  coor_extended$tc <- coor_extended$orig %>% 
    mutate(dblcoor_x = dblcoor_x,
           dblcoor_y = dblcoor_y + nmax_y)
  coor_extended$tr <- coor_extended$orig %>% 
    mutate(dblcoor_x = dblcoor_x + nmax_x,
           dblcoor_y = dblcoor_y + nmax_y)
  coor_extended$cl <- coor_extended$orig %>% 
    mutate(dblcoor_x = dblcoor_x - nmax_x,
           dblcoor_y = dblcoor_y)
  coor_extended$cr <- coor_extended$orig %>% 
    mutate(dblcoor_x = dblcoor_x + nmax_x,
           dblcoor_y = dblcoor_y)
  coor_extended$bl <- coor_extended$orig %>% 
    mutate(dblcoor_x = dblcoor_x - nmax_x,
           dblcoor_y = dblcoor_y - nmax_y)
  coor_extended$bc <- coor_extended$orig %>% 
    mutate(dblcoor_x = dblcoor_x,
           dblcoor_y = dblcoor_y - nmax_y)
  coor_extended$br <- coor_extended$orig %>% 
    mutate(dblcoor_x = dblcoor_x + nmax_x,
           dblcoor_y = dblcoor_y - nmax_y)
  coor_extended <- bind_rows(coor_extended)
  
  
  # identify coordinates of neighbor cells
  f.hex_neighb_coor <- function(x, y) {
    neighbor_pos <- tribble(
      ~n_name,     ~d_dblcoor_x, ~d_dblcoor_y,
      "nID_r1n01",           0L,           2L,
      "nID_r1n02",           1L,           1L,
      "nID_r1n03",           1L,          -1L,
      "nID_r1n04",           0L,          -2L,
      "nID_r1n05",          -1L,          -1L,
      "nID_r1n06",          -1L,           1L,
      "nID_r2n01",           0L,           4L,
      "nID_r2n02",           1L,           3L,
      "nID_r2n03",           2L,           2L,
      "nID_r2n04",           2L,           0L,
      "nID_r2n05",           2L,          -2L,
      "nID_r2n06",           1L,          -3L,
      "nID_r2n07",           0L,          -4L,
      "nID_r2n08",          -1L,          -3L,
      "nID_r2n09",          -2L,          -2L,
      "nID_r2n10",          -2L,           0L,
      "nID_r2n11",          -2L,           2L,
      "nID_r2n12",          -1L,           3L
    )
    
    res <- neighbor_pos %>% 
      mutate(n_dblcoor_x = x + d_dblcoor_x,
             n_dblcoor_y = y + d_dblcoor_y) %>% 
      select(-c(d_dblcoor_x, d_dblcoor_y))
    
    return(res)
  }
  
  coor_neighbors <- coor %>%
    select(cell_ID, dblcoor_x, dblcoor_y) %>% 
    distinct() %>% 
    mutate(neighbors = map2(dblcoor_x, dblcoor_y,
                            f.hex_neighb_coor)) %>% 
    unnest(neighbors)
  
  # get IDs of neighbor cells and store them as separate columns
  neighbor_ids_col <- coor_neighbors %>% 
    left_join(coor_extended,
              by     = c("n_dblcoor_x" = "dblcoor_x", "n_dblcoor_y" = "dblcoor_y"),
              suffix = c("", "_n")) %>% 
    select(-c(n_dblcoor_x, n_dblcoor_y)) %>% 
    pivot_wider(names_from  = n_name,
                values_from = cell_ID_n) %>% 
    select(cell_ID, starts_with("nID"))
  
  # store neighbor IDs as vector in listcolumns
  neighbor_ids_vec <- neighbor_ids_col %>%  
    rowwise() %>%
    mutate(nIDs_r1 = list(c(nID_r1n01, 
                            nID_r1n02, 
                            nID_r1n03, 
                            nID_r1n04, 
                            nID_r1n05, 
                            nID_r1n06)),
           nIDs_r2 = list(c(nID_r2n01,
                            nID_r2n02,
                            nID_r2n03,
                            nID_r2n04,
                            nID_r2n05,
                            nID_r2n06,
                            nID_r2n07,
                            nID_r2n08,
                            nID_r2n09,
                            nID_r2n10,
                            nID_r2n11,
                            nID_r2n12))) %>% 
    ungroup() %>% 
    select(cell_ID, nIDs_r1, nIDs_r2)
  
  return(neighbor_ids_vec)
}



f.init_create_simtable <- function(dat_init = init_data,
                                   v_nids   = neighbor_ids_vec,
                                   p        = param) {
  ## Inputs
  # dat_init: initialization data
  # v_nids:   neighbor IDs as vectors in df
  # p:        whole parameter list
  
  # Output
  # step_df: data frame for iterative calculations
  
  ## cell constants
  # cell and cohort IDs
  step_df_cc <- tibble(cc_ID      = dat_init$cell_ID,
                       co_Nr      = dat_init$cohort_Nr) %>%
    mutate(co_ID = str_c(cc_ID, "_", co_Nr))
  
  # Topo: take from init-file or add
  if ("Topo" %in% colnames(dat_init)) {
    step_df_cc$cc_Topo <- dat_init$Topo
  } else {
    temp_topo <- tibble(cc_ID = 1:max(dat_init$cell_ID)) %>% 
      mutate(cc_Topo = sample(x       = c("even", "mound", "pit"),
                              size    = max(dat_init$cell_ID),
                              prob    = c(p$topo_sha_even_all,
                                          p$topo_sha_mound_all,
                                          p$topo_sha_pit_all),
                              replace = TRUE))
    step_df_cc <- left_join(step_df_cc, temp_topo, by = "cc_ID")
  }
  
  # CQ (cell quality): take from init-file or add
  if ("cell_Quality" %in% colnames(dat_init)) {
    step_df_cc$cc_CQ <- dat_init$cell_Quality
  } else {
    temp_CQ <- tibble(cc_ID = 1:max(dat_init$cell_ID)) %>% 
      mutate(cc_CQ = round(runif(n   = max(dat_init$cell_ID),
                                      min = p$cq_min_all,
                                      max = p$cq_max_all),
                                3))
    step_df_cc <- left_join(step_df_cc, temp_CQ, by = "cc_ID")
  }
  
  # neighbors
  step_df_cc <- left_join(step_df_cc, v_nids,
                          by = c("cc_ID" = "cell_ID")) %>% 
    rename(cc_nIDs_r1 = nIDs_r1,
           cc_nIDs_r2 = nIDs_r2)
  
  ## time
  step_df_time <- tibble(time_Step = rep(0, p$sim_ncoh_tot))
  
  ## state 1 (before vegetation period)
  step_df_s1 <- tibble(s1_Spc = rep(NA, p$sim_ncoh_tot),
                       s1_N   = NA,
                       s1_D   = NA,
                       s1_H   = NA,
                       s1_HCB = NA)
  
  ## auxiliary variables of s1
  step_df_aux1 <- tibble(aux1_Stage = rep(NA, p$sim_ncoh_tot),
                         aux1_BA    = NA,
                         aux1_Vol   = NA,
                         aux1_CR    = NA,
                         aux1_CIh   = NA,
                         aux1_CId   = NA,
                         aux1_CIm   = NA,
                         aux1_CIc   = NA)
  
  ## changes in state variables (dynamics)
  step_df_dyn <- tibble(dyn_Ningr       = rep(NA, p$sim_ncoh_tot),
                        dyn_Nmort       = NA,
                        dyn_temp_change = NA,
                        dyn_temp_state  = NA,
                        dyn_H           = NA,
                        dyn_D           = NA,
                        dyn_HCB         = NA)
  
  ## changes in state variables (flags)
  step_df_flag <- tibble(flag_Germ            = rep(NA, p$sim_ncoh_tot),
                         flag_CohortDeath_nat = NA,
                         flag_AdReg_nat       = NA)
  
  ## state 2 (after vegetation period)
  step_df_s2 <- tibble(s2_Spc = dat_init$Species,
                       s2_N   = dat_init$N,
                       s2_D   = dat_init$D,
                       s2_H   = dat_init$H,
                       s2_HCB = dat_init$HCB)
  
  ## auxiliary variables of s2
  step_df_aux2 <- tibble(aux2_Stage = rep(NA, p$sim_ncoh_tot),
                         aux2_BA    = NA,
                         aux2_Vol   = NA,
                         aux2_CR    = NA)
  
  ## management (between vegetation periods)
  step_df_mgm <- tibble(mgm_N                = rep(NA, p$sim_ncoh_tot),
                        flag_CohortDeath_mgm = NA,
                        flag_AdReg_mgm       = NA)
  
  step_df <- bind_cols(step_df_cc,
                       step_df_time,
                       step_df_s1,
                       step_df_aux1,
                       step_df_dyn,
                       step_df_flag,
                       step_df_s2,
                       step_df_aux2,
                       step_df_mgm)
  
  return(step_df)
}
