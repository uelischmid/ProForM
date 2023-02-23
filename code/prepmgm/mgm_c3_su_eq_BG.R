## create mgm files for c3_su_eq_BG simulations
## 27.1.23, us

# setup -------------------------------------------------------------------
library(tidyverse)

template_file <- "data/processed/mgm/0_mgm_single_tree_selection_template.csv"
out_folder <- "data/processed/mgm/c3_su/"
dir.create(out_folder)

times <- tribble(
  ~mgmtime, ~time_start,
     "060",         60L,
     "075",         75L,
     "160",        160L
  )


# read template -----------------------------------------------------------
mgm_template <- read_delim(template_file,
                          delim = ";",
                          col_types = cols())


# create mgm files --------------------------------------------------------
for (i in 1:nrow(times)) {
  mgm_new <- mgm_template
  
  # values to use
  vals <- times[i,] %>% 
    mutate(across(everything(), as.character))
  
  # new file name
  filename_new <- str_c("mgm_c3_su_eq_BG_UM_t", pull(vals, mgmtime), ".csv")
  
  # start time
  mgm_new[[1, 2]] <- pull(vals, time_start)
  
  # interval
  mgm_new[[2, 2]] <- "1000"
  
  # intensity
  mgm_new[[3, 2]] <- "1"
  
  # min D
  mgm_new[[4, 2]] <- "0"
  
  # min species shares
  mgm_new[[5, 2]] <- "0"
  mgm_new[[6, 2]] <- "1"
  mgm_new[[7, 2]] <- "1"
  mgm_new[[8, 2]] <- "0"
  
  # buffer
  mgm_new[[9, 2]] <- "0"
  
  # weights
  mgm_new[[10, 2]] <- "0" # spc
  mgm_new[[11, 2]] <- "100" # BA
  mgm_new[[12, 2]] <- "0" # CR
  
  # save mgm file
  write_delim(x     = mgm_new,
              file  = str_c(out_folder, filename_new),
              delim = ";")
}

