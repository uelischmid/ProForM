################
##    MODEL   ##
## FUNCTIONS  ##
##   UPDATE   ##
##   STATES   ##
## 02.06.2022 ##
################



# species -----------------------------------------------------------------
f.update_Spc <- function(Spc_previous,
                         flag_Germ, flag_CohortDeath, flag_AdReg,
                         spc_selection_probs) {
  ## Inputs
  # Spc_previous: species in previous state
  # flag_Germ
  # flag_CohortDeath & flag_AdReg: either _nat or _mgm
  # spc_selection_probs: probabilites to select each species
  
  ## Output
  # Spc_updated: updated Species
  
  if (sum(c(flag_Germ, flag_CohortDeath, flag_AdReg)) == 0) {
    Spc_updated <- Spc_previous
    
  } else if (sum(c(flag_CohortDeath, flag_AdReg)) == 1) { # cannot be only AdReg
    Spc_updated <- "none"
    
  } else {
    Spc_updated <- sample(x    = names(spc_selection_probs),
                          size = 1,
                          prob = spc_selection_probs)
  }
  return(Spc_updated)
}


# stem number -------------------------------------------------------------
f.update_N <- function(N_previous, dyn_Ningr, dyn_Nmort, mgm_N) {
  ## Inputs
  # N_previous: N of the previous state
  # dyn_Ningr: number of ingrowing stems
  # dyn_Nmort: number of dying stems
  # mgm_N: number of harvested stems
  
  ## Output
  # N_updated: updated state variable N
  
  N_updated <- N_previous + dyn_Ningr - dyn_Nmort - mgm_N
  return(N_updated)
}


# diameter ----------------------------------------------------------------
f.update_D <- function(D_previous, dyn_D, flag_CohortDeath) {
  ## Inputs
  # D_previous: D of the previous state
  # dyn_D: diameter growth
  # flag_CohortDeath: did all trees of a cohort die? (natural death or mgm)
  
  ## Output
  # D_updated: updated state variable D
  
  if (flag_CohortDeath == FALSE) { # not all trees are removed or died
    D_updated <- D_previous + dyn_D
    
  } else { # all trees are removed or died (regardless of advance regeneration)
    D_updated <- 0
  }
  
  return(D_updated)
}


# height ------------------------------------------------------------------
f.update_H <- function(H_previous, dyn_H,
                       flag_Germ, flag_CohortDeath, flag_AdReg,
                       p = param)   {
  ## Inputs
  # H_previous: H of the previous state
  # dyn_H: height growth
  # flag_Germ: does germination occur?
  # flag_CohortDeath: did all trees of a cohort die? (natural death or mgm)
  # flag_AdReg: is there advance regeneration present?
  # p: whole parameter list
  
  ## Output
  # H_updated: updated state variable H
  
  if (flag_Germ == TRUE) { # germination occurs
    H_updated <- p$germ_h_all
    
  } else { 
    if (flag_CohortDeath == FALSE) { # growth
      H_updated <- H_previous + dyn_H
      
    } else { # all trees were harvested or died
      if (flag_AdReg == FALSE) { # no advance regeneration present
        H_updated <- 0
        
      } else { # advance regeneration present
        H_updated <- p$adreg_h_all
      }
    }
  }
  
  return(H_updated)
}


# height to crown base ----------------------------------------------------
f.update_HCB <- function(HCB_previous, dyn_HCB,
                         Spc_updated,
                         flag_CohortDeath, flag_AdReg,
                         p = param) {
  ## Inputs
  # HCB_previous: HCB of the previous state
  # dyn_HCB: change in height to crown base
  # Spc_updated: updated Species of the current state (for advance regeneration)
  # flag_CohortDeath: did all trees of a cohort die? (natural death or mgm)
  # flag_AdReg: is there advance regeneration present?
  # p: whole parameter list
  
  ## Output
  # HCB_updated: updated state variable HCB
  
  if (flag_CohortDeath == FALSE) { # not all trees are removed or have died
    HCB_updated <- HCB_previous + dyn_HCB
    
  } else { # all trees are harvested or have died
    if (flag_AdReg == FALSE) { # no advance regeneration present
      HCB_updated <- 0
      
    } else { # advance regeneration present
      if (Spc_updated == "pabi") {
        HCB_updated <- runif(1, min = 0, max = (1 - p$cb_crlimit_pabi) * p$adreg_h_all)
        
      } else if (Spc_updated == "aalb") {
        HCB_updated <- runif(1, min = 0, max = (1 - p$cb_crlimit_aalb) * p$adreg_h_all)
        
      } else if (Spc_updated == "fsyl") {
        HCB_updated <- runif(1, min = 0, max = (1 - p$cb_crlimit_fsyl) * p$adreg_h_all)
        
      } else if (Spc_updated == "apse") {
        HCB_updated <- runif(1, min = 0, max = (1 - p$cb_crlimit_apse) * p$adreg_h_all)
      }
    }
  }
  
  return(HCB_updated)
}