################
## BATCH RUNS ##
##     OF     ##
##    MODEL   ##
################


# setup -------------------------------------------------------------------
library(tidyverse)
split_vector <- function(x, n = 3) split(x, cut(seq_along(x), n, labels = FALSE))

subfolder_simsettings <- "c3_su/" # subfolder within simsettings/-folder
files_simsettings <- list.files(paste0("data/processed/simsettings/", subfolder_simsettings))
files_simsettings <- files_simsettings[str_detect(files_simsettings, ".csv")]
files_simsettings <- files_simsettings[str_detect(files_simsettings, "c3_su_eq_mgm")]

# files_simsettings <- split_vector(files_simsettings)
# files_simsettings <- files_simsettings[[3]]

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
