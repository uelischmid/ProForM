#####################
##   BATCH RUNS    ##
##    OF  NAIS     ##
##   PREPARATION   ##
#####################

# setup -------------------------------------------------------------------
library(tidyverse)
split_vector <- function(x, n = 3) split(x, cut(seq_along(x), n, labels = FALSE))

simulations <- list.dirs("data/processed/simoutput", full.names = FALSE, recursive = FALSE)
simulations <- simulations[str_detect(simulations, "c3_ST_def")]

# simulations <- split_vector(simulations)
# simulations <- simulations[[3]]

# preparation: define time window and resolution
t_starts <- rep(1, length(simulations)) # 1 if from start
t_ends <- rep(NA, length(simulations)) # NA if to the end
t_ress <- rep(1, length(simulations))

# preparation: suffix for out_folder in data/processed/naisoutput/1_prep/; NA for no suffix
out_suffixes <- rep(NA, length(simulations))


# run preparation ---------------------------------------------------------
source("code/nais/nais_prep.R")

time_start <- Sys.time()
for (i in seq_along(simulations)) {
  cat(str_c("PREPARE ", i, "/", length(simulations), ": ", simulations[i], "\n"))
  PROFORM_NaiS_prep(simulation = simulations[i],
                    t_start    = t_starts[i],
                    t_end      = t_ends[i],
                    t_res      = t_ress[i],
                    out_suffix = out_suffixes[i])
}
time_end <- Sys.time()
cat("\nBatch run ended.\n",
    length(simulations), " preparations in ", round(difftime(time_end, time_start, units = "mins"), 0), " minutes.\n",
    "On average ", round(difftime(time_end, time_start, units = "secs") / length(simulations), 0), " seconds per preparation",
    sep = "")