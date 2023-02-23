## create mgm files for c3 simulations
## single tree selection STS
## 28.1.23, us

# setup -------------------------------------------------------------------
library(tidyverse)

template_file <- "data/processed/mgm/0_mgm_single_tree_selection_template.csv"
out_folder <- "data/processed/mgm/c3_mgm/"
dir.create(out_folder)

combinations <- expand_grid(stratum       = c("UM", "HM", "SA"),
                            time_start    = 1,
                            interval      = seq(10, 40, 10),
                            intensity_val = seq(0.1, 0.4, 0.1),
                            min_D         = 12,
                            buffer        = 1,
                            w_spc         = 10,
                            w_BA          = 100,
                            w_CR          = 40) %>% 
  mutate(intensity_name = intensity_val * 100)

min_spc_shares <- tribble(
  ~stratum, ~aalb, ~apse, ~fsyl, ~pabi,
  "UM", "0.3", "0.1", "0.4",   "0",
  "HM", "0.5",   "0",   "0", "0.3",
  "SA",   "0",   "0",   "0",   "0"
)

combinations <- combinations %>%
  left_join(min_spc_shares, by = "stratum")


# read template -----------------------------------------------------------
mgm_template <- read_delim(template_file,
                           delim     = ";",
                           col_types = cols())


# create mgm files --------------------------------------------------------
for (i in 1:nrow(combinations)) {
  mgm_new <- mgm_template
  
  # values to use
  vals <- combinations[i,] %>% 
    mutate(across(everything(), as.character))
  
  # new file name
  filename_new <- str_c("mgm_STS_",
                        pull(vals, stratum), "_",
                        "t", pull(vals, interval), "_",
                        "i", pull(vals, intensity_name),
                        ".csv")
  
  # start time
  mgm_new[[1, 2]] <- pull(vals, time_start)
  
  # interval
  mgm_new[[2, 2]] <- pull(vals, interval)
  
  # intensity
  mgm_new[[3, 2]] <- pull(vals, intensity_val)
  
  # min D
  mgm_new[[4, 2]] <- pull(vals, min_D)
  
  # min species shares
  mgm_new[[5, 2]] <- pull(vals, aalb)
  mgm_new[[6, 2]] <- pull(vals, apse)
  mgm_new[[7, 2]] <- pull(vals, fsyl)
  mgm_new[[8, 2]] <- pull(vals, pabi)
  
  # buffer
  mgm_new[[9, 2]] <- pull(vals, buffer)
  
  # weights
  mgm_new[[10, 2]] <- pull(vals, w_spc) # spc
  mgm_new[[11, 2]] <- pull(vals, w_BA) # BA
  mgm_new[[12, 2]] <- pull(vals, w_CR) # CR
  
  # save mgm file
  write_delim(x     = mgm_new,
              file  = str_c(out_folder, filename_new),
              delim = ";")
}

