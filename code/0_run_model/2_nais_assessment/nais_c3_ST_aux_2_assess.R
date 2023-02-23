#####################
##   BATCH RUNS    ##
##    OF  NAIS     ##
##   ASSESSMENTS   ##
#####################

# setup -------------------------------------------------------------------
library(tidyverse)
split_vector <- function(x, n = 3) split(x, cut(seq_along(x), n, labels = FALSE))

nais_preps <- list.dirs("data/processed/naisoutput/1_prep", full.names = FALSE, recursive = FALSE)
nais_preps <- rep(nais_preps[str_detect(nais_preps, "c3_ST_aux")], 2)

# assessment: natural hazard
# 'none', 'A' (avalanche), 'LED' (landslide, erosion, debris flow), 'RF' (rockfall), or 'TF' (torrents and floods)
nat_haz <- c(rep("A", length(nais_preps) / 2),
             rep("LED", length(nais_preps) / 2))

# assessment: rockfall scenario
# NA or name of scenario if natural hazard is rockfall ('rs1', 'rs2sl1', 'rs2sl2')
rockfall_scenarios <- rep(NA, length(nais_preps))


# nais_preps <- split_vector(nais_preps)
# nais_preps <- nais_preps[[3]]
# 
# nat_haz <- split_vector(nat_haz)
# nat_haz <- nat_haz[[3]]
# 
# rockfall_scenarios <- split_vector(rockfall_scenarios)
# rockfall_scenarios <- rockfall_scenarios[[3]]


# run assessment ----------------------------------------------------------
source("code/nais/nais_assess.R")
for (i in seq_along(nais_preps)) {
  cat(str_c("ASSESS ", i, "/", length(nais_preps), ": ", nais_preps[i], "\n"))
  PROFORM_NaiS_assess(nais_prep         = nais_preps[i],
                      natural_hazard    = nat_haz[i],
                      rockfall_scenario = rockfall_scenarios[i])
}
