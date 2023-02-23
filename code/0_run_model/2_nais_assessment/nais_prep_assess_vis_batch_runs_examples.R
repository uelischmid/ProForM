#####################
##   BATCH RUNS    ##
##    OF  NAIS     ##
##   PREPARATION,  ##
##  ASSESSMENT, &  ##
##  VISUALIZATION  ##
#####################


# setup -------------------------------------------------------------------
library(tidyverse)

# define steps to be done
do_prep <- TRUE # preparation
do_assess <- TRUE # assessment
do_vis_F2 <- TRUE # visualization of F2
do_vis_si <- TRUE # visualization of subindices

# simulations: manually list simulations
simulations <- c("HM_example_mgm",
                 "HM_example_nomgm")

# # simulations: filter simulations by name
# simulations <- list.dirs("data/processed/simoutput", full.names = FALSE, recursive = FALSE)
# simulations <- simulations[str_detect(simulations, "HM_example")]


# preparation: define time window and resolution
t_starts <- rep(1, length(simulations)) # 1 if from start
t_ends <- rep(NA, length(simulations)) # NA if to the end
t_ress <- rep(1, length(simulations))

# preparation: suffix for out_folder in data/processed/naisoutput/1_prep/; NA for no suffix
out_suffixes <- rep(NA, length(simulations))

# assessment: natural hazard
# 'none', 'A' (avalanche), 'LED' (landslide, erosion, debris flow), 'RF' (rockfall), or 'TF' (torrents and floods)
nat_haz <- rep("A", length(simulations))

# assessment: rockfall scenario
# NA or name of scenario if natural hazard is rockfall ('rs1', 'rs2sl1', 'rs2sl2')
rockfall_scenarios <- rep(NA, length(simulations))

# visualization of F2
avgs <- rep(3, length(simulations)) # NA for no averaging or number of time_Steps else
out_F2 <- rep("examples", length(simulations)) # subfolder for outputs within results/nais_vis/1_sim_F2/, NA if none

# visualization of subindices
out_si <- rep("examples", length(simulations)) # subfolder for outputs within results/nais_vis/1_sim_subind/, NA if none


# run preparation ---------------------------------------------------------
if (do_prep == TRUE) {
  source("code/nais/nais_prep.R")
  for (i in seq_along(simulations)) {
    cat(str_c("PREPARE ", i, "/", length(simulations), ": ", simulations[i], "\n"))
    PROFORM_NaiS_prep(simulation = simulations[i],
                      t_start    = t_starts[i],
                      t_end      = t_ends[i],
                      t_res      = t_ress[i],
                      out_suffix = out_suffixes[i])
  }
}


# run assessment ----------------------------------------------------------
nais_preps <- vector(mode = "character", length = length(simulations))
for (i in seq_along(simulations)) {
  if (is.na(out_suffixes[i])) {
    nais_preps[i] <- simulations[i]
  } else {
    nais_preps[i] <- str_c(simulations[i], "_", out_suffixes[i])
  }
}

if (do_assess == TRUE) {
  source("code/nais/nais_assess.R")
  for (i in seq_along(nais_preps)) {
    cat(str_c("ASSESS ", i, "/", length(nais_preps), ": ", nais_preps[i], "\n"))
    PROFORM_NaiS_assess(nais_prep         = nais_preps[i],
                        natural_hazard    = nat_haz[i],
                        rockfall_scenario = rockfall_scenarios[i])
  }
}


# run visualization F2 ----------------------------------------------------
nais_assessments <- str_c(nais_preps, "_", nat_haz)

if (do_vis_F2 == TRUE) {
  source("code/visualization/vis_1_nais_F2.R")
  for (i in seq_along(nais_assessments)) {
    cat(str_c("VIS F2 ", i, "/", length(nais_assessments), ": ", nais_assessments[i], "\n"))
    PROFORM_vis_1_nais_F2(nais_assessment = nais_assessments[i],
                          avg_across      = avgs[i],
                          out_subfolder   = out_F2[i])
  }
}


# run visualization subindices --------------------------------------------
nais_assessments <- str_c(nais_preps, "_", nat_haz)

if (do_vis_si == TRUE) {
  source("code/visualization/vis_1_nais_subindices.R")
  for (i in seq_along(nais_assessments)) {
    cat(str_c("VIS SUBINDICES ", i, "/", length(nais_assessments), ": ", nais_assessments[i], "\n"))
    PROFORM_vis_1_nais_si(nais_assessment = nais_assessments[i],
                          out_subfolder   = out_si[i])
  }
}


