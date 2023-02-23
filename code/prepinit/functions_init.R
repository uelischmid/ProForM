####
## Functions to generate init-data
## 18.1.22, us
###


# get trees of first EFM-inventory ----------------------------------------
f.get_EFM_inv1 <- function(df) {
  ## Input
  # df: data frame of inventory (plot$trees)
  
  ## Output
  # res: data frame of first inventory with diameter classes
  
  res <- df %>% 
    filter(year == min(year)) %>% 
    filter(status %in% c(1, 2)) %>% 
    mutate(Dclass_4 = cut_width(diameter, width = 4, boundary = 8, closed = "left")) %>% 
    arrange(desc(diameter))
  
  return(res)
}



# load parameters ---------------------------------------------------------
f.get_parameters <- function(param_name, str) {
  ## Inputs
  # param_name: name of param-file
  # str:        selected stratum
  
  ## Output
  # res: parameters as list
  
  res_temp <- read_delim(str_c("data/processed/paramfordyn/", param_name), ";",
                         col_types = cols(
                           `function` = col_character(),
                           f.expl = col_character(),
                           stratum = col_character(),
                           species = col_character(),
                           p.name = col_character(),
                           p.value = col_double(),
                           p.expl = col_character()
                         )) %>% 
    filter(stratum == str) %>% 
    mutate(p.name2 = str_c(p.name, "_", species))
  
  res <- res_temp %>%
    transpose() %>% 
    map("p.value")
  names(res) <- res_temp$p.name2
  
  return(res)
}


# filter and rename species -----------------------------------------------
f.filter_EFM_species <- function(inv1, spc) {
  ## Inputs
  # inv1: data frame of first inventory
  # spc:  selected species
  
  ## Outputs
  # inv1_sp: data frame of first inventory reduced to selected species and renamed
  
  inv1_sp <- inv1 %>% 
    filter(species %in% spc) %>% 
    mutate(species = case_when(species == "Abies alba"          ~ "aalb",
                               species == "Acer"                ~ "apse",
                               species == "Acer pseudoplatanus" ~ "apse",
                               species == "Picea abies"         ~ "pabi",
                               species == "Fagus sylvatica"     ~ "fsyl"))
  
  return(inv1_sp)
}



# calcualte simulation parameters --------------------------------------------
f.get_sim_list <- function(str, coh_p_cell, meta, p = param) {
  ## Inputs
  # str:        selected stratum
  # coh_p_cell: cohorts per cell
  # meta:       metadata of plot (plot$meta)
  # p:          whole parameter list
  
  ## Output
  # sim: list with simulation parameters
  
  sim <- expand_grid(nrow = c(1:40),
                     ncol = c(1:40)[c(1:40 %% 2 == 0)]) %>% 
    mutate(stratum           = str,
           ncell             = nrow * ncol,
           cellsize_m2       = p$str_cellsize_all,
           simarea_m2        = ncell * cellsize_m2,
           simarea_ha        = simarea_m2 / 10000,
           cohortspercell    = coh_p_cell,
           ncohorts          = ncell * cohortspercell,
           diff_EFM          = simarea_ha - meta$area,
           diff_EFM_abs      = abs(diff_EFM),
           diff_EFM_abs_perc = diff_EFM_abs / meta$area,
           share_EFM         = simarea_ha / meta$area,
           diff_nrow_ncol    = abs(nrow - ncol),
           efm_aspect_deg    = meta$aspect_deg,
           efm_slope_deg     = meta$slope_deg) %>% 
    arrange(diff_EFM_abs) %>% 
    filter(diff_EFM_abs_perc <= 0.02) %>%
    arrange(diff_nrow_ncol) %>% 
    head(1) %>% 
    select(stratum, cellsize_m2, nrow, ncol, ncell, simarea_m2, everything()) %>% 
    transpose() %>% 
    unlist(recursive = FALSE)
  
  return(sim)
}


f.get_sim_tbl <- function(sim_list) {
  ## Inputs
  # sim_list: simulation parameters as list
  
  ## Outputs
  # sim_tbl: simulation parameters as table
  
  sim_tbl <- bind_rows(sim_list) %>% 
    mutate(across(c(simarea_m2, diff_EFM, diff_EFM_abs), ~ round(.x, digits = 1))) %>%
    mutate(across(c(diff_EFM_abs_perc, share_EFM), ~ round(.x, digits = 2))) %>%
    mutate(across(everything(), as.character)) %>% 
    pivot_longer(everything(), names_to = "var", values_to = "val")
  
  return(sim_tbl)
}



# distribute EFM data to cohorts ------------------------------------------
f.distr_EFM <- function(spc, inv1, cf, p = param) {
  ## Inputs
  # spc:  species present in first inventory
  # inv1: data frame of first inventory
  # cf:   cell-fill-factor
  # p:    whole parameter list
  
  ## Output
  # inv1_cn: data frame of first inventory with new "temp_cohortID"
  
  cohortnumbers_v <- vector(mode = "integer")
  cohortnumber <- 0
  
  for (current_species in spc) {
    trees_inv1_species <- inv1 %>% 
      filter(species == current_species)
    
    cum_cpa <- (p$str_cellsize_all * cf)
    
    for (i in 1:nrow(trees_inv1_species)) {
      cum_cpa <- cum_cpa + trees_inv1_species[i, ]$CPA_calc_m2
      
      if (cum_cpa > (p$str_cellsize_all * cf)) {
        cum_cpa <- trees_inv1_species[i, ]$CPA_calc_m2
        cohortnumber <- cohortnumber + 1
      }
      
      cohortnumbers_v <- c(cohortnumbers_v, cohortnumber)
    }
  }
  
  inv1_cn <- inv1 %>%
    arrange(species, desc(diameter)) %>%
    mutate(temp_cohortID = cohortnumbers_v)
  
  return(inv1_cn)
}


# characterize cohorts ----------------------------------------------------
f.calc_h <- function (spc, diam, p = param) {
  ## Inputs
  # spc:  species
  # diam: DBH
  # p:    whole parameter list
  
  ## Output
  # h: height
  
  if (diam < 12) {
    if (spc == "pabi") {
      h <- (diam - p$dgreg_int_pabi) / p$dgreg_h_pabi
    } else if (spc == "aalb") {
      h <- (diam - p$dgreg_int_aalb) / p$dgreg_h_aalb
    } else if (spc == "fsyl") {
      h <- (diam - p$dgreg_int_fsyl) / p$dgreg_h_fsyl
    } else if (spc == "apse") {
      h <- (diam - p$dgreg_int_apse) / p$dgreg_h_apse
    }
  } else {
    if (spc == "pabi") {
      h <- p$hgadu_asym_pabi / (1 + exp((p$hgadu_xmid_pabi - diam) / p$hgadu_scal_pabi))
    } else if (spc == "aalb") {
      h <- p$hgadu_asym_aalb / (1 + exp((p$hgadu_xmid_aalb - diam) / p$hgadu_scal_aalb))
    } else if (spc == "fsyl") {
      h <- p$hgadu_asym_fsyl / (1 + exp((p$hgadu_xmid_fsyl - diam) / p$hgadu_scal_fsyl))
    } else if (spc == "apse") {
      h <- p$hgadu_asym_apse / (1 + exp((p$hgadu_xmid_apse - diam) / p$hgadu_scal_apse))
    }
  }
  return(h)
}


f.make_init_EFM <- function(inv1) {
  ## Inputs
  # inv1: data frame of first inventory
  
  ## Output
  # init_adu: EFM data formatted as init-file
  
  init_adu <- inv1 %>% 
    group_by(temp_cohortID) %>% 
    summarise(N       = n(),
              D       = mean(diameter),
              cr      = mean(CR_calc),
              Species = unique(species)) %>% 
    mutate(H   = pmap_dbl(list(spc  = Species,
                               diam = D),
                          f.calc_h),
           HCB = H * (1 - cr),
           BA  = N * pi * (D/200)^2)
  
  return(init_adu)
}



# compare init_adu & EFM --------------------------------------------------
f.compare_init_EFM <- function(init, inv1, sim_list) {
  ## Inputs
  # init: init data from EFM-data
  # inv1: data frame of first inventory
  # sim_list: simulation parameters as list
  
  ## Output
  ## res: list with different comparisons
  
  res <- list()
  
  # graph
  res$gg <- init %>% 
    mutate(D_class = cut_width(D, width = 4, boundary = 0, closed = "left")) %>% 
    group_by(D_class) %>% 
    summarise(N_sum = sum(N)) %>% 
    ggplot(aes(D_class, N_sum)) +
    geom_col() +
    geom_bar(data = inv1, aes(Dclass_4), color = "red",
             inherit.aes = FALSE) +
    labs(title = "Comparison of DBH-distribution",
         subtitle = "init (grey) vs. inv1 (red)",
         x = "Diameter class",
         y = "N") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90))
  
  # total BA
  res$ba_tot <- c(sum(inv1$BA_calc_m2),
                  sum(init$BA),
                  sum(init$BA) / sum(inv1$BA_calc_m2))
  names(res$ba_tot) <- c("BA_inv1", "BA_init", "BA_init_share")
  
  # BA per species
  ba1 <- inv1 %>% 
    group_by(species) %>% 
    summarise(BA_inv1 = sum(BA_calc_m2))
  ba2 <- init %>% 
    group_by(Species) %>% 
    summarise(BA_init = sum(BA))
  res$ba_spc <- left_join(ba1, ba2, by = c("species" = "Species")) %>% 
    mutate(BA_diff = BA_init - BA_inv1) %>% 
    as.data.frame()
  
  # stem numbers
  res$sn <- c(nrow(inv1), sum(init$N), sum(init$N) - nrow(inv1))
  names(res$sn) <- c("N_inv1", "N_init", "N_diff")
  
  # cells and cohorts
  res$cell_coh$shares <- c(sim_list$ncell,
                           sim_list$ncohorts,
                           nrow(init),
                           nrow(init) / sim_list$ncell,
                           nrow(init) / sim_list$ncohorts)
  names(res$cell_coh$shares) <- c("sim_ncell", "sim_ncoh", "init_ncoh",
                              "share_init_cell", "share_init_coh")
  
  res$cell_coh$n_coh_empty1 <- max(0, sim_list$ncell - nrow(init))
  
  
  if (sim_list$cohortspercell == 2) {
    res$cell_coh$n_coh_adu_to_second <- max(0, nrow(init) - sim_list$ncell)
    res$cell_coh$n_coh_empty2 <- min(sim_list$ncell, sim_list$ncohorts - nrow(init))
  }
  
  if (sim_list$cohortspercell > 2) {
    res$cell_coh$ERROR <- "ATTENTION: Function not adapted for more than 2 cohorts per cell!"
  }
  
  if (nrow(init) > sim_list$ncohorts) {
    res$cell_coh$ERROR <- "ATTENTION: Too many cohorts in init!"
  }
  
  # output
  return(res)
}



# add cohort nr to init_adu -----------------------------------------------
f.add_coh_nr_adu <- function(init, sim_list, comp_coh) {
  ## Inputs
  # init: init data from EFM-data
  # sim_list: simulation parameters as list
  # comp_coh: comparison init-EFM: cell/cohort part
  
  ## Output
  # init_cn: init data from EFM-data with additional cohort number
  
  if (nrow(init) <= sim_list$ncell) {
    # fewer or equal number of cohorts than cells: assign all to first cohort
    init_cn <- init %>% 
      mutate(cohort_Nr = 1)
  } else if (nrow(init) <= sim_list$ncohorts) {
    # more cohorts than cells: assign smallest diameter to second cohort
    init_cn <- init %>% 
      arrange(desc(D)) %>% 
      mutate(cohort_Nr = c(rep(1, times = sim_list$ncell),
                           rep(2, times = comp_coh$n_coh_adu_to_second)))
  } else {
    # more cohorts in init than in sim: error
    stop("ERROR: Too many cohorts in init!")
  }
}




# generate empty cohorts --------------------------------------------------
f.make_init_empty <- function(n_coh, coh_nr) {
  ## Inputs
  # n_coh:  number of empty cohorts to generate
  # coh_nr: cohort_Nr to assign
  
  ## Output
  # df: dataframe with empty cohorts
  
  df <- tibble(
    temp_cohortID = rep(999, times = n_coh),
    N             = 0,
    D             = 0,
    cr            = 0,
    Species       = "none",
    H             = 0,
    HCB           = 0,
    BA            = 0,
    cohort_Nr     = coh_nr
  )
  
  return(df)
}


# write description file --------------------------------------------------
f.write_descr <- function(type, str, name, sim_t, folder_out) {
  ## Inputs
  # type: 'EFM' or 'BG'; decisive for output folder
  # str: selected stratum
  # name: name of plot/output file
  # sim_t: simulation settings as table

  ## Output
  # description file in folder
  
  if (type == "EFM") {
    descr_file <- str_c("data/processed/init/EFM/init_", str, "_", name, "_descr.txt")
  } else if (type == "BG") {
    descr_file <- str_c("data/processed/init/", folder_out, "/init_", str, "_BG_", name, "_descr.txt")
  } else {
    stop("ERROR: 'type' must be either 'EFM' or 'BG")
  }
  
  if (type == "EFM") {
    cat("Simulation settings:\n",
        file = descr_file)
  } else if (type == "BG") {
    cat("Grid settings:\n",
        file = descr_file)
  }
  
  capture.output(as.data.frame(sim_t),
                 file = descr_file,
                 type = "output",
                 append = TRUE)
}



# extract init and description from simulation ----------------------------
f.sim_extract_save <- function(simname, ts, out_loc) {
  ## Inputs
  # simname: name of simulation
  # ts: time step to extract from
  # out_loc: subfolder name within init-folder
  
  ## Outputs
  # init-file from s2 saved to location (.csv)
  # descr-file with simulation settings of orig. sim (.txt)
  
  ## init
  # load data of selected time step
  res_ts <- read_rds(str_c("data/processed/simoutput/", simname, "/3_output/res_full.rds")) %>%
    filter(time_Step == ts)
  
  # extract s2 of specified time step and build init
  init <- res_ts %>%
    select(cell_ID   = cc_ID,
           cohort_Nr = co_Nr,
           Species   = s2_Spc,
           H         = s2_H,
           HCB       = s2_HCB,
           D         = s2_D,
           N         = s2_N)
  
  # save init
  write_csv(init,
            str_c("data/processed/init/", out_loc, "/", simname, "_ts", as.character(ts), ".csv"))
  
  ## description file
  # load param
  param <- read_rds(str_c("data/processed/simoutput/", simname, "/3_output/param.rds"))
  
  # assemble description
  descr <- tibble(stratum        = param$sim_stratum,
                  cellsize_m2    = param$sim_cellsize_m2,
                  nrow           = param$sim_cells_nrow,
                  ncol           = param$sim_cells_ncol,
                  ncell          = param$sim_ncells,
                  simarea_m2     = param$sim_area_m2,
                  simarea_ha     = param$sim_area_ha,
                  cohortspercell = param$sim_ncoh_cell,
                  ncohorts       = param$sim_ncoh_tot,
                  aspect         = param$sim_aspect,
                  slope          = param$sim_slope,
                  site_qual      = param$sim_qual_site,
                  reg_qual       = param$sim_qual_reg,
                  spc_sh_aalb    = param$spc_sh_aalb,
                  spc_sh_apse    = param$spc_sh_apse,
                  spc_sh_fsyl    = param$spc_sh_fsyl,
                  spc_sh_pabi    = param$spc_sh_pabi,
                  spc_pr         = param$spc_pressure) %>% 
    mutate(across(everything(), as.character)) %>% 
    pivot_longer(everything(),
                 names_to  = "var",
                 values_to = "val")
  
  # write description file
  descr_file <- str_c("data/processed/init/", out_loc, "/", simname, "_ts", as.character(ts), "_descr.txt")
  cat("Simulation settings:\n",
      file = descr_file)
  capture.output(as.data.frame(descr),
                 file   = descr_file,
                 type   = "output",
                 append = TRUE)
}

f.sim_extract_save2 <- function(simname, ts, out_loc, out_name) {
  ## Inputs
  # simname: name of simulation
  # ts: time step to extract from
  # out_loc: subfolder name within init-folder
  
  ## Outputs
  # init-file from s2 saved to location (.csv)
  # descr-file with simulation settings of orig. sim (.txt)
  
  ## init
  # load data of selected time step
  res_ts <- read_rds(str_c("data/processed/simoutput/", simname, "/3_output/res_full.rds")) %>%
    filter(time_Step == ts)
  
  # load param
  param <- read_rds(str_c("data/processed/simoutput/", simname, "/3_output/param.rds"))
  
  # extract s2 of specified time step and build init
  init <- res_ts %>%
    select(cell_ID   = cc_ID,
           cohort_Nr = co_Nr,
           Species   = s1_Spc,
           H         = s1_H,
           HCB       = s1_HCB,
           D         = s1_D,
           N         = s1_N)
  
  # save init
  write_csv(init,
            str_c("data/processed/init/", out_loc, "/", out_name, ".csv"))
  
  ## description file
  descr <- tibble(stratum        = param$sim_stratum,
                  cellsize_m2    = param$sim_cellsize_m2,
                  nrow           = param$sim_cells_nrow,
                  ncol           = param$sim_cells_ncol,
                  ncell          = param$sim_ncells,
                  simarea_m2     = param$sim_area_m2,
                  simarea_ha     = param$sim_area_ha,
                  cohortspercell = param$sim_ncoh_cell,
                  ncohorts       = param$sim_ncoh_tot,
                  aspect         = param$sim_aspect,
                  slope          = param$sim_slope,
                  site_qual      = param$sim_qual_site,
                  reg_qual       = param$sim_qual_reg,
                  spc_sh_aalb    = param$spc_sh_aalb,
                  spc_sh_apse    = param$spc_sh_apse,
                  spc_sh_fsyl    = param$spc_sh_fsyl,
                  spc_sh_pabi    = param$spc_sh_pabi,
                  spc_pr         = param$spc_pressure) %>% 
    mutate(across(everything(), as.character)) %>% 
    pivot_longer(everything(),
                 names_to  = "var",
                 values_to = "val")
  
  # write description file
  descr_file <- str_c("data/processed/init/", out_loc, "/", out_name, "_descr.txt")
  cat("Simulation settings:\n",
      file = descr_file)
  capture.output(as.data.frame(descr),
                 file   = descr_file,
                 type   = "output",
                 append = TRUE)
}
