#####################
##   BATCH RUNS    ##
##       OF        ##
## NAIS ASSESSMENT ##
#####################

# setup -------------------------------------------------------------------
# simulations names: names of folders in .../simoutput
simulations <- c("SM_example_mgm",
                 "SM_example_nomgm",
                 "UM_example_mgm",
                 "UM_example_nomgm",
                 "HM_example_mgm",
                 "HM_example_nomgm",
                 "SA_example_mgm",
                 "SA_example_nomgm")

# strata fine: specify HM1, HM2, SA1, or SA2 if necessary. NA otherwise
strata_fine <- c(NA, NA,
                 NA, NA,
                 "HM1", "HM1",
                 "SA1", "SA1")

# natural hazards: 
# 'none', 'A' (avalanche), 'LED' (landslide, erosion, debris flow), 'RF' (rockfall), or 'TF' (torrents and floods)
natural_hazards <- c("LED", "LED",
                     "TF", "TF",
                     "RF", "RF",
                     "A", "A")

# rockfall scenarios:
# NA or name of scenario if natural hazard is rockfall ('rs1', 'rs2sl1', 'rs2sl2')
rockfall_scenarios <- c(NA, NA,
                        NA, NA,
                        "rs2sl1", "rs2sl1",
                        NA, NA)

# time resolution
time_starts <- rep(1, length(simulations)) # 1 if from start
time_ends <- rep(NA, length(simulations)) # NA if to the end
time_resolutions <- rep(1, length(simulations))


# assess nais -------------------------------------------------------------
suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(magrittr)))
source("code/nais/nais_assessment.R")

time_start <- Sys.time()
for (i in seq_along(simulations)) {
  cat(paste0("ASSESS ", i, "/", length(simulations), ": ", simulations[i], "\n"))
  PROFORM_assess_NaiS(simulation        = simulations[i],
                      stratum_fine      = strata_fine[i],
                      natural_hazard    = natural_hazards[i],
                      rockfall_scenario = rockfall_scenarios[i],
                      time_resolution   = time_resolutions[i],
                      time_start        = time_starts[i],
                      time_end          = time_ends[i])
  cat("\n")
}

time_end <- Sys.time()
cat("\nBatch run ended.\n",
    length(simulations), " NaiS-assessments in ", round(difftime(time_end, time_start, units = "mins"), 0), " minutes.\n",
    "On average ", round(difftime(time_end, time_start, units = "secs") / length(simulations), 0), " seconds per NaiS-assessment.",
    sep = "")


