################
##    MODEL   ##
## FUNCTIONS  ##
##   FOREST   ##
##  DYNAMICS  ##
##   & FLAGS  ##
## 02.09.2021 ##
################



# dyn: height growth ------------------------------------------------------
f.dyn_hgreg <- function(Species, H, CIh, Topo, p = param) {
  ## Inputs
  # Species, H, CIh, Topo
  # p: whole parameter list
  
  ## Output
  # dyn_H for trees with D < 12 cm
  
  if (Species == "pabi") {
    dyn_H_log <- 
      p$hgreg_int_pabi +
      p$hgreg_c_h_pabi * log(H) +
      p$hgreg_c_ci_pabi * CIh +
      p$hgreg_c_dd1_pabi * p$hgreg_v_dd1_all +
      p$hgreg_c_spei1_pabi * p$hgreg_v_spei1_all +
      p$hgreg_c_spei2_pabi * p$hgreg_v_spei2_all
    
    if (Topo == "mound") {
      dyn_H_log <- dyn_H_log + p$hgreg_c_topo_mound_pabi
    } else if (Topo == "pit") {
      dyn_H_log <- dyn_H_log + p$hgreg_c_topo_pit_pabi
    }
    
    dyn_H_temp <- exp(dyn_H_log)
    
  } else if (Species == "aalb") {
    dyn_H_log <- 
      p$hgreg_int_aalb +
      p$hgreg_c_h_aalb * log(H) +
      p$hgreg_c_ci_aalb * CIh +
      p$hgreg_c_dd2_aalb * p$hgreg_v_dd2_all +
      p$hgreg_c_spei1_aalb * p$hgreg_v_spei1_all +
      p$hgreg_c_slasp_aalb * p$sim_sloasp
    
    if (Topo == "mound") {
      dyn_H_log <- dyn_H_log + p$hgreg_c_topo_mound_aalb
    } else if (Topo == "pit") {
      dyn_H_log <- dyn_H_log + p$hgreg_c_topo_pit_aalb
    }
    
    dyn_H_temp <- exp(dyn_H_log)
    
  } else if (Species == "fsyl") {
    dyn_H_log <- 
      p$hgreg_int_fsyl +
      p$hgreg_c_h_fsyl * log(H) +
      p$hgreg_c_ci_fsyl * CIh +
      p$hgreg_c_dd2_fsyl * p$hgreg_v_dd2_all +
      p$hgreg_c_spei1_fsyl * p$hgreg_v_spei1_all +
      p$hgreg_c_slasp_fsyl * p$sim_sloasp
    
    dyn_H_temp <- exp(dyn_H_log)
      
  } else if (Species == "apse") {
    dyn_H_log <- 
      p$hgreg_int_apse +
      p$hgreg_c_h_apse * log(H) +
      p$hgreg_c_ci_apse * CIh +
      p$hgreg_c_dd1_apse * p$hgreg_v_dd1_all +
      p$hgreg_c_dd2_apse * p$hgreg_v_dd2_all +
      p$hgreg_c_spei1_apse * p$hgreg_v_spei1_all +
      p$hgreg_c_spei2_apse * p$hgreg_v_spei2_all +
      p$hgreg_c_slasp_apse * p$sim_sloasp
    
    dyn_H_temp <- exp(dyn_H_log)
    
  } else {
    dyn_H_temp <- 999999999
  }
  
  # modify growth with prefactor
  dyn_H <- p$pf_dh * dyn_H_temp
  
  return(dyn_H)
}



f.dyn_hgadu <- function(Species, D, dD, p = param) {
  ## Inputs
  # Species, D,
  # dD from temp_change
  # p: whole parameter list
  
  ## Output
  # H_fut for trees with DBH > 12
  
  if (Species == "pabi") {
    H_fut <- 
      p$hgadu_asym_pabi / (1 + exp((p$hgadu_xmid_pabi - D - dD) / p$hgadu_scal_pabi))
  } else if (Species == "aalb") {
    H_fut <- 
      p$hgadu_asym_aalb / (1 + exp((p$hgadu_xmid_aalb - D - dD) / p$hgadu_scal_aalb))
  } else if (Species == "fsyl") {
    H_fut <- 
      p$hgadu_asym_fsyl / (1 + exp((p$hgadu_xmid_fsyl - D - dD) / p$hgadu_scal_fsyl))
  } else if (Species == "apse") {
    H_fut <- 
      p$hgadu_asym_apse / (1 + exp((p$hgadu_xmid_apse - D - dD) / p$hgadu_scal_apse))
  } else {
    H_fut <- 99999
  }
  
  return(H_fut)
}


f.dyn_hgall <- function(Stage, temp_change, temp_state, H) {
  ## Inputs
  # Stage, temp_change, temp_state, Height

  ## Output
  # dyn_H for all trees
  
  if (Stage %in% c(0:3)) {
    dyn_H <- temp_change
  } else {
    dyn_H <- temp_state - H
  }
  
  dyn_H <- max(0, dyn_H)
  
  return(dyn_H)
}


# dyn: diameter growth ----------------------------------------------------
f.dyn_dgreg <- function(Species, H, dH, p = param) {
  ## Inputs
  # Species, H
  # dH from temp_change
  # p: whole parameter list
  
  ## Output
  # D_fut for trees with DBH < 12 cm
  
  if (Species == "pabi") {
    D_fut_temp <- 
      p$dgreg_int_pabi +
      p$dgreg_h_pabi * (H + dH)
    
    D_fut <- max(c(0, D_fut_temp))
  } else if (Species == "aalb") {
    D_fut_temp <- 
      p$dgreg_int_aalb +
      p$dgreg_h_aalb * (H + dH)
    
    D_fut <- max(c(0, D_fut_temp))
  } else if (Species == "fsyl") {
    D_fut_temp <- 
      p$dgreg_int_fsyl +
      p$dgreg_h_fsyl * (H + dH)
    
    D_fut <- max(c(0, D_fut_temp))
  } else if (Species == "apse") {
    D_fut_temp <- 
      p$dgreg_int_apse +
      p$dgreg_h_apse * (H + dH)
    
    D_fut <- max(c(0, D_fut_temp))
  } else {
    D_fut <- 999999999
  }
  
  return(D_fut)
}



if ("pabi" %in% species_parametrized) {
  f.dyn_dgadu_ddpot_pabi <- approxfun(c(param$dgadu_dmaxfull_pabi, param$dgadu_dmax0_pabi),
                                      c(param$dgadu_ddmax_pabi, 0),
                                      rule = 2)
}

if ("aalb" %in% species_parametrized) {
  f.dyn_dgadu_ddpot_aalb <- approxfun(c(param$dgadu_dmaxfull_aalb, param$dgadu_dmax0_aalb),
                                      c(param$dgadu_ddmax_aalb, 0),
                                      rule = 2)
}

if ("fsyl" %in% species_parametrized) {
  f.dyn_dgadu_ddpot_fsyl <- approxfun(c(param$dgadu_dmaxfull_fsyl, param$dgadu_dmax0_fsyl),
                                      c(param$dgadu_ddmax_fsyl, 0),
                                      rule = 2)
}

if ("apse" %in% species_parametrized) {
  f.dyn_dgadu_ddpot_apse <- approxfun(c(param$dgadu_dmaxfull_apse, param$dgadu_dmax0_apse),
                                      c(param$dgadu_ddmax_apse, 0),
                                      rule = 2)
}


f.dyn_dgadu <- function(Species, D, CId, p = param) {
  ## Inputs
  # Species, D, CId
  # p: whole parameter list
  
  ## Output
  # dyn_D for trees with D >= 12 cm
  
  # calculate potential growth
  if (Species == "pabi") {
    dyn_D_pot <- f.dyn_dgadu_ddpot_pabi(D)
    
  } else if (Species == "aalb") {
    dyn_D_pot <- f.dyn_dgadu_ddpot_aalb(D)
    
  } else if (Species == "fsyl") {
    dyn_D_pot <- f.dyn_dgadu_ddpot_fsyl(D)
    
  } else if (Species == "apse") {
    dyn_D_pot <- f.dyn_dgadu_ddpot_apse(D)
    
  } else {
    dyn_D_pot <- 0
  }
  
  # calculate reduction factor
  RFdD <- max(1 - CId / p$dgadu_cidmax_all, 0)^p$dgadu_cidexp_all
  
  # calculate ideal growth
  dyn_D_ideal <- RFdD * dyn_D_pot
  
  # reduce by prefactor
  dyn_D <- p$pf_dd * dyn_D_ideal
  
  return(dyn_D)
}


f.dyn_dgall <- function(Stage, temp_change, temp_state, D) {
  ## Inputs
  # Stage, temp_change, temp_state, Diameter
  
  ## Output
  # dyn_D for all trees
  
  if (Stage %in% c(0:3)) {
    dyn_D <- temp_state - D
  } else {
    dyn_D <- temp_change
  }
  
  return(dyn_D)
}


# dyn: stem number change -------------------------------------------------
f.dyn_ingr <- function(Cell_ID, Stage, H, dyn_H, Species, CQ,
                       vec_H_cell_temp = temp_H_cell,
                       p = param) {
  ## Inputs
  # Cell_ID: ID of cell
  # Stage, H, dyn_H, Species: Cohort variables
  # CQ: cell quality
  # vec_H_cell_temp: height of all cells as vector
  # p: whole parameter list
  
  # Output
  # dyn_Ningr
  
  if(Stage < 3 && H + dyn_H >= 130) { # "ingrowth" into stage 3
    if (Species == "pabi") {
      nmax <- if_else(condition = vec_H_cell_temp[Cell_ID] > 0,
                      true      = p$ingr_nmax_pabi / p$sim_ncoh_cell,
                      false     = p$ingr_nmax_pabi)
    } else if (Species == "aalb") {
      nmax <- if_else(condition = vec_H_cell_temp[Cell_ID] > 0,
                      true      = p$ingr_nmax_aalb / p$sim_ncoh_cell,
                      false     = p$ingr_nmax_aalb)
    } else if (Species == "fsyl") {
      nmax <- if_else(condition = vec_H_cell_temp[Cell_ID] > 0,
                      true      = p$ingr_nmax_fsyl / p$sim_ncoh_cell,
                      false     = p$ingr_nmax_fsyl)
    } else if (Species == "apse") {
      nmax <- if_else(condition = vec_H_cell_temp[Cell_ID] > 0,
                      true      = p$ingr_nmax_apse / p$sim_ncoh_cell,
                      false     = p$ingr_nmax_apse)
    }
    dyn_Ningr <- round(p$pf_gi * nmax * CQ * p$sim_cellsize_m2)
  } else {
    dyn_Ningr <- 0
  }
  
  return(dyn_Ningr)
}


f.dyn_mort <- function(N, Species, D, dyn_D, CIm, p = param) {
  ## Inputs
  # N, Species, D, dyn_D, CIm
  # p: whole parameter list
  
  ## Output
  # dyn_Nmort
  
  if (N > 0) {
    if (Species == "pabi") {
      if (D < 4) { # constant mortality rate below 4 cm DBH
        p_mort <- p$mort_preg_pabi
        
      } else { # empirical mortality function above 4 cm DBH
        D <- D * 10 # cm to mm
        dyn_D <- dyn_D * 10 # cm to mm
        CIm <- CIm * 10 # cm to mm
        
        relBAI <- (D + dyn_D)^2 / D^2 - 1
        
        XBeta <- 
          p$mort_int_pabi +
          p$mort_D_pabi * log(D) +
          p$mort_relBAI_pabi * DescTools::LogSt(relBAI, threshold = p$mort_c_all) +
          p$mort_mD_pabi * log(CIm)
        attributes(XBeta) <- NULL
        
        p_mort <- exp(XBeta) / (1 + exp(XBeta))
      }
      dyn_Nmort <- sum(runif(N) < p_mort)
      
    } else if (Species == "aalb") {
      if (D < 4) { # constant mortality rate below 4 cm DBH
        p_mort <- p$mort_preg_aalb
        
      } else { # empirical mortality function above 4 cm DBH
        D <- D * 10 # cm to mm
        dyn_D <- dyn_D * 10 # cm to mm

        relBAI <- (D + dyn_D)^2 / D^2 - 1
        
        XBeta <- 
          p$mort_int_aalb +
          p$mort_D_aalb * log(D) +
          p$mort_relBAI_aalb * DescTools::LogSt(relBAI, threshold = p$mort_c_all) +
          p$mort_relBAI2_aalb * DescTools::LogSt(relBAI, threshold = p$mort_c_all)^2
        attributes(XBeta) <- NULL
        
        p_mort <- exp(XBeta) / (1 + exp(XBeta))
      }
      dyn_Nmort <- sum(runif(N) < p_mort)
      
    } else if (Species == "fsyl") {
      if (D < 4) { # constant mortality rate below 4 cm DBH
        p_mort <- p$mort_preg_fsyl
        
      } else { # empirical mortality function above 4 cm DBH
        D <- D * 10 # cm to mm
        dyn_D <- dyn_D * 10 # cm to mm
        
        relBAI <- (D + dyn_D)^2 / D^2 - 1
        
        XBeta <- 
          p$mort_int_fsyl +
          p$mort_D_fsyl * log(D) +
          p$mort_relBAI_fsyl * DescTools::LogSt(relBAI, threshold = p$mort_c_all) +
          p$mort_relBAI2_fsyl * DescTools::LogSt(relBAI, threshold = p$mort_c_all)^2
        attributes(XBeta) <- NULL
        
        p_mort <- exp(XBeta) / (1 + exp(XBeta))
      }
      dyn_Nmort <- sum(runif(N) < p_mort)
      
    } else if (Species == "apse") {
      if (D < 4) { # constant mortality rate below 4 cm DBH
        p_mort <- p$mort_preg_apse
        
      } else { # empirical mortality function above 4 cm DBH
        D <- D * 10 # cm to mm
        dyn_D <- dyn_D * 10 # cm to mm

        relBAI <- (D + dyn_D)^2 / D^2 - 1
        
        XBeta <- 
          p$mort_int_apse +
          p$mort_D_apse * log(D) +
          p$mort_relBAI_apse * DescTools::LogSt(relBAI, threshold = p$mort_c_all)
        attributes(XBeta) <- NULL
        
        p_mort <- exp(XBeta) / (1 + exp(XBeta))
      }
      dyn_Nmort <- sum(runif(N) < p_mort)
      
    }
  } else {
    dyn_Nmort <- 0
  }
  
  return(dyn_Nmort)
}


# dyn: height to crown base change ----------------------------------------
f.dyn_hcb <- function(Species, HCB, H, dyn_H, CIc, p = param) {
  ## Inputs
  # Species, HCB, H, dyn_H, CIc
  # p: whole parameter list 
  
  ## Output
  # dyn_HCB
  
  if (Species == "none") {
    dyn_HCB <- 0
  } else {
    if (Species == "pabi") {
      dyn_HCB_max_H <- min(c(p$cb_dhcbmax_abs_pabi, p$cb_dhcbmax_rel_pabi * H)) # max depending on H
      dyn_HCB_max_CR <- (1 - p$cb_crlimit_pabi) * (H + dyn_H) - HCB # max depending on CR-limit
      
      dyn_HCB_min_H <- p$cb_dhcbmin_rel_pabi * dyn_HCB_max_H * (-1) # min depending on max
      dyn_HCB_min_CR <- HCB * (-1) # min depending on HCB
      
      f.dyn_HCB_temp <- approxfun(c(0, p$cb_cicthreshold1_pabi, p$cb_cicthreshold2_pabi),
                                  c(dyn_HCB_min_H, 0, dyn_HCB_max_H),
                                  rule = 2)
      
    } else if (Species == "aalb") {
      dyn_HCB_max_H <- min(c(p$cb_dhcbmax_abs_aalb, p$cb_dhcbmax_rel_aalb * H)) # max depending on H
      dyn_HCB_max_CR <- (1 - p$cb_crlimit_aalb) * (H + dyn_H) - HCB # max depending on CR-limit
      
      dyn_HCB_min_H <- p$cb_dhcbmin_rel_aalb * dyn_HCB_max_H * (-1) # min depending on max
      dyn_HCB_min_CR <- HCB * (-1) # min depending on HCB
      
      f.dyn_HCB_temp <- approxfun(c(0, p$cb_cicthreshold1_aalb, p$cb_cicthreshold2_aalb),
                                  c(dyn_HCB_min_H, 0, dyn_HCB_max_H),
                                  rule = 2)
      
    } else if (Species == "fsyl") {
      dyn_HCB_max_H <- min(c(p$cb_dhcbmax_abs_fsyl, p$cb_dhcbmax_rel_fsyl * H)) # max depending on H
      dyn_HCB_max_CR <- (1 - p$cb_crlimit_fsyl) * (H + dyn_H) - HCB # max depending on CR-limit
      
      dyn_HCB_min_H <- p$cb_dhcbmin_rel_fsyl * dyn_HCB_max_H * (-1) # min depending on max
      dyn_HCB_min_CR <- HCB * (-1) # min depending on HCB
      
      f.dyn_HCB_temp <- approxfun(c(0, p$cb_cicthreshold1_fsyl, p$cb_cicthreshold2_fsyl),
                                  c(dyn_HCB_min_H, 0, dyn_HCB_max_H),
                                  rule = 2)
      
    } else if (Species == "apse") {
      dyn_HCB_max_H <- min(c(p$cb_dhcbmax_abs_apse, p$cb_dhcbmax_rel_apse * H)) # max depending on H
      dyn_HCB_max_CR <- (1 - p$cb_crlimit_apse) * (H + dyn_H) - HCB # max depending on CR-limit
      
      dyn_HCB_min_H <- p$cb_dhcbmin_rel_apse * dyn_HCB_max_H * (-1) # min depending on max
      dyn_HCB_min_CR <- HCB * (-1) # min depending on HCB
      
      f.dyn_HCB_temp <- approxfun(c(0, p$cb_cicthreshold1_apse, p$cb_cicthreshold2_apse),
                                  c(dyn_HCB_min_H, 0, dyn_HCB_max_H),
                                  rule = 2)
      
    }
    
    dyn_HCB_temp <- f.dyn_HCB_temp(CIc)
    
    if (dyn_HCB_temp < 0) {
      dyn_HCB <- max(dyn_HCB_temp, dyn_HCB_min_CR)
    } else {
      dyn_HCB <- min(dyn_HCB_temp, dyn_HCB_max_CR)
    }
  }
  
  return(dyn_HCB)
}


# flags -------------------------------------------------------------------
f.flag_germ <- function(Cell_ID, Stage,
                        vec_H_cell_temp = temp_H_cell,
                        p               = param) {
  ## Inputs
  # Cell_ID: ID of cell
  # Stage of cell
  # vec_H_cell_temp: height of all cells as vector
  # p: whole parameter list
  
  ## Output
  # flag_Germ
  
  flag_Germ <- FALSE
  
  if (Stage == 0) {
    germ_p_temp1 <- 1 - (1 - p$germ_cumprob_all)^(1 / p$germ_tgerm_all)
    
    # reduce germination probability, if there are already trees on a cell
    if (vec_H_cell_temp[Cell_ID] > 0) {
      germ_p_temp2 <- germ_p_temp1 / p$sim_ncoh_cell
    } else {
      germ_p_temp2 <- germ_p_temp1
    }
    
    # modify germination probability with prefactor
    germ_p <- p$pf_gi * germ_p_temp2
    
    # decide whether germination happens
    if (runif(1) <= germ_p) flag_Germ <- TRUE
  }
  
  return(flag_Germ)
}


f.flag_cohortdeath <- function(N, dyn_N) {
  ## Inputs
  # N, dyn_N (can be either dyn_Nmort or mgm_N)
  
  # Output
  # flag_CohortDeath (can be either flag_CohortDeath_nat or flag_CohortDeath_mgm)
  
  if (N > 0 && abs(N) == abs(dyn_N)) {
    flag_CohortDeath <- TRUE
  } else {
    flag_CohortDeath <- FALSE
  }
  
  return(flag_CohortDeath)
}


f.flag_adreg <- function(Stage, flag_CohortDeath, p = param) {
  ## Inputs
  # Stage, flag_CohortDeath (can be either flag_CohortDeath_nat or flag_CohortDeath_mgm)
  # p: whole parameter list
  
  ## Output
  # flag_AdReg (can be either flag_AdReg_nat or flag_AdReg_mgm)
  
  flag_AdReg <- FALSE
  
  if (Stage == 4 && flag_CohortDeath == TRUE) {
    if (runif(1) <= (p$pf_gi * p$adreg_p_s4_all)) flag_AdReg <- TRUE
    
  } else if (Stage == 5 && flag_CohortDeath == TRUE) {
    if (runif(1) <= (p$pf_gi * p$adreg_p_s5_all)) flag_AdReg <- TRUE
    
  } else if (Stage == 6 && flag_CohortDeath == TRUE) {
    if (runif(1) <= (p$pf_gi * p$adreg_p_s6_all)) flag_AdReg <- TRUE
    
  } else if (Stage == 7 && flag_CohortDeath == TRUE) {
    if (runif(1) <= (p$pf_gi * p$adreg_p_s7_all)) flag_AdReg <- TRUE
    
  } else if (Stage == 8 && flag_CohortDeath == TRUE) {
    if (runif(1) <= (p$pf_gi * p$adreg_p_s8_all)) flag_AdReg <- TRUE
    
  }
  
  return(flag_AdReg)
}
