################
##    NAIS    ##
##  PROFILES  ##
## 09.01.2023 ##
################

## required inputs
# param$sim_stratum # from sim-output
# param$sim_slope # from sim-output
# param$sim_qual_site # from sim-output
# natural_hazard # 'none', 'A' (avalanche), 'LED' (landslide, erosion, debris flow), 'RF' (rockfall), or 'TF' (torrents and floods)
# rockfall_scenario # NA, 'rs1', 'rs2sl1', 'rs2sl2'

nais_profile <- list()

# SM ----------------------------------------------------------------------
if (param$sim_stratum == "SM") {
  # species mixture
  # vectors of length 4 with lower [1] and upper [2] limits for ideal and minimal [3, 4] profile
  nais_profile$mixture$aalb <- NA
  nais_profile$mixture$apse <- c(0.1, 0.5, 0.01, 0.7)
  nais_profile$mixture$fsyl <- c(0.5, 0.9, 0.3, 0.1)
  nais_profile$mixture$pabi <- NA
  nais_profile$mixture$coni <- c(0, 0, 0, 0.3)
  
  # crown ratio
  # vectors of length 2 with limits for ideal [1] and minimal [2] profile
  nais_profile$suptr_cr$aalb <- c(NA, NA)
  nais_profile$suptr_cr$pabi <- c(NA, NA)
  nais_profile$suptr_cr$coni <- c(NA, NA)
  
  # seedlings
  # share of "gap-cells" with seedlings, limit for ideal [1] and minimal [2] profile (deprecated)
  nais_profile$seedl$share <- c(1, 1 * 0.8)
  # species that should be present
  nais_profile$seedl$species <- c("fsyl")
  # number of species that should be present for ideal [1] and minimal [2] profile
  nais_profile$seedl$species_n <- c(1, 1)
  
  # saplings & thicket
  nais_profile$sapthi <- nais_profile$mixture # canopy cover equals basal area share of mixture
  nais_profile$sapthi$tot <- c(0.13, 0.05) # total cc of st, limit for ideal [1] and minimal [2] profile
  
  # diameter distribution
  # vectors of length 4 with limits for D1 to D4
  if (param$sim_qual_site <= 3) {
    d234 <- c(0.22, 0.16, 0.15)
  } else {
    d234 <- c(0.38, 0.18, 0.07)
  }
  nais_profile$va_dc$ideal <- c(nais_profile$sapthi$tot[1], d234)
  nais_profile$va_dc$minimal <- c(nais_profile$sapthi$tot[2], d234)
  
  # canopy cover reduction factor
  nais_profile$cc_red <- 1
}


# UM ----------------------------------------------------------------------
if (param$sim_stratum == "UM") {
  # species mixture
  # vectors of length 4 with lower [1] and upper [2] limits for ideal and minimal [3, 4] profile
  nais_profile$mixture$aalb <- c(0.3, 0.5, 0.1, 0.6)
  nais_profile$mixture$apse <- c(0.1, 0.3, 0.01, 0.6)
  nais_profile$mixture$fsyl <- c(0.4, 0.6, 0.3, 0.8)
  nais_profile$mixture$pabi <- c(0, 0.2, 0, 0.3)
  nais_profile$mixture$coni <- NA
  
  if (natural_hazard == "LED") {
    nais_profile$mixture$aalb <- c(0.3, 0.5, 0.2, 0.6)
  } 
  if (natural_hazard == "A") {
    nais_profile$mixture$coni <- c(0.3, 0.7, 0.3, 0.7)
  }
  
  # crown ratio
  # vectors of length 2 with limits for ideal [1] and minimal [2] profile
  nais_profile$suptr_cr$aalb <- c(NA, 0.66)
  nais_profile$suptr_cr$pabi <- c(NA, 0.5)
  nais_profile$suptr_cr$coni <- c(0.67, NA)
  
  # seedlings
  # share of "gap-cells" with seedlings, limit for ideal [1] and minimal [2] profile (deprecated)
  nais_profile$seedl$share <- c(1, 1 * 0.8)
  # species that should be present
  nais_profile$seedl$species <- c("aalb", "apse", "fsyl")
  # number of species that should be present for ideal [1] and minimal [2] profile
  nais_profile$seedl$species_n <- c(3, 2)
  
  # saplings & thicket
  nais_profile$sapthi <- nais_profile$mixture # canopy cover equals basal area share of mixture
  nais_profile$sapthi$tot <- c(0.12, 0.07) # total cc of st, limit for ideal [1] and minimal [2] profile
  
  # diameter distribution
  # vectors of length 4 with limits for D1 to D4
  if (param$sim_qual_site <= 3) {
    d234 <- c(0.2, 0.14, 0.13)
  } else {
    d234 <- c(0.27, 0.15, 0.15)
  }
  nais_profile$va_dc$ideal <- c(nais_profile$sapthi$tot[1], d234)
  nais_profile$va_dc$minimal <- c(nais_profile$sapthi$tot[2], d234)
  
  # canopy cover reduction factor
  nais_profile$cc_red <- 1
}


# HM ----------------------------------------------------------------------
if (param$sim_stratum == "HM") { # Haupt- und Nebenareal
  # species mixture
  # vectors of length 4 with lower [1] and upper [2] limits for ideal and minimal [3, 4] profile
  nais_profile$mixture$aalb <- c(0.5, 0.7, 0.3, 0.9)
  nais_profile$mixture$apse <- NA
  nais_profile$mixture$fsyl <- NA
  nais_profile$mixture$pabi <- c(0.3, 0.5, 0.1, 0.7)
  nais_profile$mixture$coni <- NA
  
  # crown ratio
  # vectors of length 2 with limits for ideal [1] and minimal [2] profile
  nais_profile$suptr_cr$aalb <- c(NA, NA)
  nais_profile$suptr_cr$pabi <- c(NA, NA)
  nais_profile$suptr_cr$coni <- c(0.67, 0.5)
  
  # seedlings
  # share of "gap-cells" with seedlings, limit for ideal [1] and minimal [2] profile (deprecated)
  nais_profile$seedl$share <- c(0.9, 0.9 * 0.8)
  # species that should be present
  nais_profile$seedl$species <- c("aalb", "pabi")
  # number of species that should be present for ideal [1] and minimal [2] profile
  nais_profile$seedl$species_n <- c(2, 1)
  
  # saplings & thicket
  nais_profile$sapthi <- nais_profile$mixture # canopy cover equals basal area share of mixture
  nais_profile$sapthi$tot <- c(0.10, 0.07) # total cc of st, limit for ideal [1] and minimal [2] profile
  
  # diameter distribution
  # vectors of length 4 with limits for D1 to D4
  if (param$sim_qual_site <= 3) {
    d234 <- c(0.17, 0.16, 0.24)
  } else {
    d234 <- c(0.18, 0.17, 0.23)
  }
  nais_profile$va_dc$ideal <- c(nais_profile$sapthi$tot[1], d234)
  nais_profile$va_dc$minimal <- c(nais_profile$sapthi$tot[2], d234)
  
  # canopy cover reduction factor
  nais_profile$cc_red <- 0.9
}



# SA ----------------------------------------------------------------------
if (param$sim_stratum == "SA") {
  # species mixture
  # vectors of length 4 with lower [1] and upper [2] limits for ideal and minimal [3, 4] profile
  nais_profile$mixture$aalb <- NA
  nais_profile$mixture$apse <- NA
  nais_profile$mixture$fsyl <- NA
  nais_profile$mixture$pabi <- c(0, 1, 0, 1)
  nais_profile$mixture$coni <- NA
  
  # crown ratio
  # vectors of length 2 with limits for ideal [1] and minimal [2] profile
  nais_profile$suptr_cr$aalb <- c(NA, NA)
  nais_profile$suptr_cr$pabi <- c(NA, NA)
  nais_profile$suptr_cr$coni <- c(1, 0.67)
  
  # seedlings
  # share of "gap-cells" with seedlings, limit for ideal [1] and minimal [2] profile (deprecated)
  nais_profile$seedl$share <- c(0.7, 0.7 * 0.8)
  # species that should be present
  nais_profile$seedl$species <- c("pabi")
  # number of species that should be present for ideal [1] and minimal [2] profile
  nais_profile$seedl$species_n <- c(1, 1)
  
  # saplings & thicket
  nais_profile$sapthi <- nais_profile$mixture # canopy cover equals basal area share of mixture
  nais_profile$sapthi$tot <- c(0.16, 0.12) # total cc of st, limit for ideal [1] and minimal [2] profile
  
  # diameter distribution
  # vectors of length 4 with limits for D1 to D4
  if (param$sim_qual_site <= 3) {
    d234 <- c(0.17, 0.12, 0.13)
  } else {
    d234 <- c(0.2, 0.14, 0.14)
  }
  nais_profile$va_dc$ideal <- c(nais_profile$sapthi$tot[1], d234)
  nais_profile$va_dc$minimal <- c(nais_profile$sapthi$tot[2], d234)
  
  # canopy cover reduction factor
  nais_profile$cc_red <- 0.75
}


# avalanche ---------------------------------------------------------------
if (natural_hazard == "A") {
  # gap length
  gaplengths_slope <- tribble(
    ~slope, ~min, ~ide,
    0L,  Inf,  Inf,
    30L,   60,   50,
    35L,   50,   40,
    40L,   40,   30,
    45L,   30,   25
  )
  
  if (param$sim_stratum %in% c("SM", "UM")) {
    gaplengths_slope <- gaplengths_slope[-2,]
  }
  
  gaplengths <- gaplengths_slope %>% 
    filter(slope <= param$sim_slope) %>% 
    slice_max(order_by = slope, n = 1) %>%
    transpose() %>% 
    magrittr::extract2(1)
  
  nais_profile$A$gaplength <- c(gaplengths$ide, gaplengths$min) # limit for ideal [1] and minimal [2] profile
  rm(gaplengths_slope, gaplengths)
  
  # gap width
  if (param$sim_stratum %in% c("HM", "SA")) {
    nais_profile$A$gapwidth <- 15
  } else {
    nais_profile$A$gapwidth <- 5
  }
  
  # canopy cover
  nais_profile$A$cc <- 0.5
}


# landslides, erosion, debris flow ----------------------------------------
if (natural_hazard == "LED") {
  # min. gap width for SA1 & SA2 to be considered
  nais_profile$LED$gapwidth_SA <- 20
  
  # gap area
  # vector of length 2 with limits for ideal [1] and minimal [2] profile
  nais_profile$LED$gaparea_insuff_reg <- c(400, 600) # not enough regeneration
  nais_profile$LED$gaparea_suff_reg <- c(800, 1200) # sufficient regeneration
  
  # canopy cover
  # vector of length 2 with limits for ideal [1] and minimal [2] profile
  nais_profile$LED$cc <- c(0.6, 0.4)
}


# rockfall ----------------------------------------------------------------
if (natural_hazard == "RF") {
  # stem numbers and basal area scenarios
  if (rockfall_scenario == "rs1") {
    nais_profile$RF$n_ba_limits <- tribble(
      ~cat, ~min, ~ide,
      "N1", 310L, 380L,
      "N2", 310L, 390L,
      "N3", 110L, 140L,
      "N4",  20L,  30L,
      "BA",  20L,  25L
    )
    
  } else if (rockfall_scenario == "rs2sl1") {
    nais_profile$RF$n_ba_limits <- tribble(
      ~cat, ~min, ~ide,
      "N1", 440L, 490L,
      "N2", 450L, 500L,
      "N3", 150L, 170L,
      "N4", 120L, 140L,
      "BA",  45L,  50L
    )
    
  } else if (rockfall_scenario == "rs2sl2") {
    nais_profile$RF$n_ba_limits <- tribble(
      ~cat, ~min, ~ide,
      "N1", 310L, 360L,
      "N2", 320L, 370L,
      "N3", 110L, 130L,
      "N4",  90L, 100L,
      "BA",  32L,  37L
    )
  } 
  
  # gap length
  nais_profile$RF$gaplength_ext <- 40
}


# torrents and floods -----------------------------------------------------
if (natural_hazard == "TF") {
  # gap length
  # vector of length 2 with limits for ideal [1] and minimal [2] profile
  nais_profile$TF$gaplength <- c(20, 30)
  
  # gap area
  # vector of length 2 with limits for ideal [1] and minimal [2] profile
  nais_profile$TF$gaparea <- c(600, 1200)
  
  # canopy cover
  # vector of length 2 with limits for ideal [1] and minimal [2] profile
  nais_profile$TF$cc <- c(0.6, 0.5)
}

