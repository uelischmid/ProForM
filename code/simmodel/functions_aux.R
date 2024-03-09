################
##    MODEL   ##
## FUNCTIONS  ##
## AUXILIARY  ##
## VARIABLES  ##
## 31.08.2021 ##
################



f.aux_stage <- function(H, D) {
  ## Inputs
  # H & D
  
  ## Output
  # Stage
  
  if (H == 0) {
    Stage <- 0L # bare ground
  } else if (H < 40) {
    Stage <- 1L # seedlings ("Anwuchs")
  } else if (H < 130) {
    Stage <- 2L # saplings ("Aufwuchs")
  } else if (D < 12) {
    Stage <- 3L # thicket ("Dickung")
  } else if (D < 20) {
    Stage <- 4L # poles1 ("Stangenholz 1")
  } else if (D < 30) {
    Stage <- 5L # poles2 ("Stangenholz 2")
  } else if (D < 40) {
    Stage <- 6L # timber1 ("Baumholz 1")
  } else if (D < 50) {
    Stage <- 7L # timber2 ("Baumholz 2")
  } else {
    Stage <- 8L # timber3 ("Baumholz 3")
  }
  
  return(Stage)
  
}


f.aux_ba <- function(N, D) {
  ## Inputs
  # N & D
  
  ## Output
  # BA
  
  BA <- N * pi * (0.5 * D / 100)^2
  
  return(BA)
}

f.aux_vol <- function(N, D) {
  ## Inputs
  # N & D
  
  ## Output
  # Vol according to Denzin-Formula
  
  Vol <- N * D^2 / 1000
  
  return(Vol)
}

f.aux_cr <- function(H, HCB) {
  ## Inputs
  # H & HCB
  
  ## Output
  # Crown ratio CR
  
  CR <- (H - HCB) / H
  
  return(CR)
}


f.aux_CIh <- function(Cell_ID, Species,
                      vec_nr1, vec_nr2,
                      vec_BA_cell_temp = temp_BA_cell,
                      p = param) {
  ## Inputs
  # Cell_ID: ID of cell
  # Species: cohort species
  # vec_nr1 & vec_nr2: vectors of neighboring cells in ring 1 and 2
  # vec_BA_cell_temp: basal area of all cells as vector
  # p: whole parameter list
  
  ## Output
  # CIh (weighted basal area)
  
  # extract BA of target cell and of neighbors
  BA_target <- vec_BA_cell_temp[Cell_ID]
  BA_nr1 <- vec_BA_cell_temp[vec_nr1]
  BA_nr2 <- vec_BA_cell_temp[vec_nr2]
  
  if (Species == "none") {
    CIh <- 99999
    
  } else if (Species == "fsyl") { # beech: weighted by angle
    w0 <- (p$cih_wmax_fsyl - 1) * 0.5 + 1

    CIh <- (w0 * BA_target + sum(BA_nr1 * p$cih_vec_weights_angle)) /
      (p$cihd_ncells_all * p$sim_cellsize_m2 / 10000)
    
  } else { # other species: weighted by distance
    # calculate weights corresponding to species and stratum
    if (p$sim_stratum %in% c("SM", "UM")) {
      if(Species == "pabi") {
        w0 <- (p$cih_wmax_pabi - 1) * 0.75 + 1
        w1 <- (p$cih_wmax_pabi - 1) * 0.25 + 1
        w2 <- 0
      } else if (Species == "aalb") {
        w0 <- (p$cih_wmax_aalb - 1) * 0.75 + 1
        w1 <- (p$cih_wmax_aalb - 1) * 0.25 + 1
        w2 <- 0
      } else if (Species == "apse") {
        w0 <- (p$cih_wmax_apse - 1) * 0.75 + 1
        w1 <- (p$cih_wmax_apse - 1) * 0.25 + 1
        w2 <- 0
      }
    } else { # strata HM & SA
      if(Species == "pabi") {
        w0 <- 0
        w1 <- (p$cih_wmax_pabi - 1) * 0.75 + 1
        w2 <- (p$cih_wmax_pabi - 1) * 0.25 + 1
      } else if (Species == "aalb") {
        w0 <- 0
        w1 <- (p$cih_wmax_aalb - 1) * 0.75 + 1
        w2 <- (p$cih_wmax_aalb - 1) * 0.25 + 1
      }
    }
    
    # calculate CIh
    CIh <- (w0 * BA_target + w1 * sum(BA_nr1) + w2 * sum(BA_nr2)) /
      (p$cihd_ncells_all * p$sim_cellsize_m2 / 10000)
  }
  
  return(CIh)
}

f.aux_CId <- function(ID, Species, D,
                      vec_nr1, vec_nr2,
                      vec_BA_cell_temp = temp_BA_cell,
                      p = param) {
  ## Inputs
  # ID: cell ID
  # Species: cohort species
  # D: cohort DBH
  # vec_nr1 & vec_nr2: vectors of neighboring cells in ring 1 and 2
  # vec_BA_cell_temp: basal area of all cells as vector
  # p: whole parameter list
  
  ## Output
  # CId (weighted basal area)
  # SM & UM: corresponds to CIh weighted by distance with (potentially)
  #          different weight but with one tree less in the target cohort on the target cell
  # HM & SA: corresponds to CIh weighted by distance with (potentially)
  #          different weights plus the focus cell itself minus the 
  #          "target tree" in the target cohort, weighed with the max. weight
  
  BA_focuscell_red <- vec_BA_cell_temp[ID] - f.aux_ba(N = 1, D = D)
  if (is.na(BA_focuscell_red)) BA_focuscell_red <- 0
  
  BA_nr1 <- vec_BA_cell_temp[vec_nr1]
  BA_nr2 <- vec_BA_cell_temp[vec_nr2]
  
  if (p$sim_stratum %in% c("SM", "UM")) {
    if (Species == "none") {
      CId <- 99999
    } else {
      if (Species == "pabi") {
        # assign weights
        w0 <- (p$cid_wmax_pabi - 1) * 0.75 + 1
        w1 <- (p$cid_wmax_pabi - 1) * 0.25 + 1
        w2 <- 0
        
      } else if (Species == "aalb") {
        # assign weights
        w0 <- (p$cid_wmax_aalb - 1) * 0.75 + 1
        w1 <- (p$cid_wmax_aalb - 1) * 0.25 + 1
        w2 <- 0
        
      } else if (Species == "fsyl") {
        # assign weights
        w0 <- (p$cid_wmax_fsyl - 1) * 0.75 + 1
        w1 <- (p$cid_wmax_fsyl - 1) * 0.25 + 1
        w2 <- 0
        
      } else if (Species == "apse") {
        # assign weights
        w0 <- (p$cid_wmax_apse - 1) * 0.75 + 1
        w1 <- (p$cid_wmax_apse - 1) * 0.25 + 1
        w2 <- 0
        
      }
      # calculate CId
      CId <- (w0 * BA_focuscell_red + w1 * sum(BA_nr1) + w2 * sum (BA_nr2)) /
        (p$cihd_ncells_all * p$sim_cellsize_m2 / 10000)
      
    }
  } else { # strata HM & SA
    if (Species == "none") {
      CId <- 99999
    } else {
      if (Species == "aalb") {
        # assign weights
        w0 <- p$cid_wmax_aalb
        w1 <- (p$cid_wmax_aalb - 1) * 0.75 + 1
        w2 <- (p$cid_wmax_aalb - 1) * 0.25 + 1
        
      } else if (Species == "pabi") {
        # assign weights
        w0 <- p$cid_wmax_pabi
        w1 <- (p$cid_wmax_pabi - 1) * 0.75 + 1
        w2 <- (p$cid_wmax_pabi - 1) * 0.25 + 1
        
      }
      # calculate CId
      CId <- (w0 * BA_focuscell_red + w1 * sum(BA_nr1) + w2 * sum (BA_nr2)) /
        (p$cihd_ncells_all * p$sim_cellsize_m2 / 10000)
      
    }
  }
  return(CId)
}



f.aux_CIm <- function(df) {
  ## Input
  # dataframe with columns s1_N and s1_D

  ## Output
  # CIm (mean diameter of trees with D > 7 cm)

  df <- df %>%
    filter(s1_D >= 7)

  if (nrow(df) > 0) {
    CIm <- weighted.mean(x = df$s1_D, w = df$s1_N)
  } else {
    CIm <- 1
  }

  return(CIm)
}


f.aux_CIc <- function(Species, D,
                      vec_nr1,
                      temp_vec_IDs = temp_IDs,
                      temp_vec_D   = temp_D_cohorts,
                      temp_vec_BA  = temp_BA_cohorts,
                      p       = param) {
  ## Input
  # Species: cohort species
  # D: cohort diameter
  # vec_nr1: vectors of neighboring cells in ring 1
  # p: whole parameter list
  
  ## Output
  # CIc (BA/ha of trees larger than frac * D)
  
  # Boolean vector of neighboring cohorts
  neighboring_cohort_positions <- temp_vec_IDs %in% vec_nr1
  
  # Boolean vector of cohorts with large enough D
  if (Species == "pabi") {
    cohorts_largeenough_D <- temp_vec_D > (p$cic_frac_pabi * D)
  } else if (Species == "aalb") {
    cohorts_largeenough_D <- temp_vec_D > (p$cic_frac_aalb * D)
  } else if (Species == "fsyl") {
    cohorts_largeenough_D <- temp_vec_D > (p$cic_frac_fsyl * D)
  } else if (Species == "apse") {
    cohorts_largeenough_D <- temp_vec_D > (p$cic_frac_apse * D)
  } else {
    cohorts_largeenough_D <- rep(FALSE, length(temp_vec_D))
  }
  
  # sum up cohorts that are neighbors AND have large enough D
  CIc <- sum(temp_vec_BA[neighboring_cohort_positions & cohorts_largeenough_D]) /
    (6 * p$sim_cellsize_m2 / 10000)

  return(CIc)
}



f.aux_spcprob <- function(Species, D, BA, p = param) {
  ## Input
  # Species: Species-column of step_df
  # D: D-column of step_df
  # BA: BA-column of step_df
  # p: whole parameter list
  
  ## Output
  # Species selection probabilities at germination
  
  BA_distr <- bind_cols(Species = Species,
                        D       = D,
                        BA      = BA) %>% 
    filter(D >= p$spc_dmin_all) %>% 
    mutate(Species = factor(Species, levels = names(p$spc_shares))) %>% 
    group_by(Species, .drop = FALSE) %>% 
    summarise(BA_spc = sum(BA))
  if (sum(BA_distr$BA_spc) == 0) {
    p_sel_spc <- p$spc_shares
  } else {
    p_sel_spc <- BA_distr %>% 
      mutate(share_curr = BA_spc / sum(BA_spc),
             share_surr = p$spc_shares / 100,
             p_sel      = share_curr + (p$spc_pressure / 100) * (share_surr - share_curr)) %>% 
      pull(p_sel, name = Species)
  }
  return(p_sel_spc)
}
