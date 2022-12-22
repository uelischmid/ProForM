################
## BATCH RUNS ##
##     OF     ##
##    MODEL   ##
################


# setup -------------------------------------------------------------------
library(tidyverse)

subfolder_simsettings <- "examples/" # subfolder within simsettings/-folder
  files_simsettings <- list.files(str_c("data/processed/simsettings/", subfolder_simsettings))

# run simulations ---------------------------------------------------------
paths_simsettings <- str_c(subfolder_simsettings, files_simsettings)
source("code/simmodel/PROFORM_model.R")

time_start <- Sys.time()
for (i in seq_along(paths_simsettings)) {
  cat(paste0("RUN ", i, "/", length(paths_simsettings), "\n"))
  PROFORM_simulate(simsettings = paths_simsettings[i])
}

time_end <- Sys.time()
cat("\nBatch run ended.\n",
    length(paths_simsettings), " simulations in ", round(difftime(time_end, time_start, units = "mins"), 0), " minutes.\n",
    "On average ", round(difftime(time_end, time_start, units = "secs") / length(paths_simsettings), 0), " seconds per simulation.",
    sep = "")
