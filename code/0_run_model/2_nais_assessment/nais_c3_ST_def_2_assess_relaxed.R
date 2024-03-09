#####################
##   BATCH RUNS    ##
##    OF  NAIS     ##
##   ASSESSMENTS   ##
#####################

# setup -------------------------------------------------------------------
library(tidyverse)
split_vector <- function(x, n = 3) split(x, cut(seq_along(x), n, labels = FALSE))

nais_preps <- list.dirs("data/processed/naisoutput/1_prep", full.names = FALSE, recursive = FALSE)
nais_preps <- nais_preps[str_detect(nais_preps, "c3_ST_def")]

nais_preps <- rep(nais_preps, 2)

# assessment: natural hazard
# 'none', 'A' (avalanche), 'LED' (landslide, erosion, debris flow), 'RF' (rockfall), or 'TF' (torrents and floods)
nat_haz <- c(rep("A", length(nais_preps) / 2),
             rep("LED", length(nais_preps) / 2))

# assessment: rockfall scenario
# NA or name of scenario if natural hazard is rockfall ('rs1', 'rs2sl1', 'rs2sl2')
rockfall_scenarios <- rep(NA, length(nais_preps))


# nais_preps <- split_vector(nais_preps)
# nais_preps <- nais_preps[[1]]
# 
# nat_haz <- split_vector(nat_haz)
# nat_haz <- nat_haz[[1]]
# 
# rockfall_scenarios <- split_vector(rockfall_scenarios)
# rockfall_scenarios <- rockfall_scenarios[[1]]


# run assessment ----------------------------------------------------------
source("code/nais/nais_assess_relaxed.R")

time_start <- Sys.time()
for (i in seq_along(nais_preps)) {
  cat(str_c("ASSESS ", i, "/", length(nais_preps), ": ", nais_preps[i], " - ", nat_haz[i],  "\n"))
  PROFORM_NaiS_assess_relaxed(nais_prep         = nais_preps[i],
                              natural_hazard    = nat_haz[i],
                              rockfall_scenario = rockfall_scenarios[i])
}
time_end <- Sys.time()
cat("\nBatch run ended.\n",
    length(nais_preps), " assessments in ", round(difftime(time_end, time_start, units = "mins"), 0), " minutes.\n",
    "On average ", round(difftime(time_end, time_start, units = "secs") / length(nais_preps), 0), " seconds per assessment",
    sep = "")
