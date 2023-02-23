## create mgm files for c3_su_eq_BG simulations
## 27.1.23, us

# setup -------------------------------------------------------------------
library(tidyverse)

template_file <- "data/processed/mgm/0_mgm_group_selection_template.csv"
out_folder <- "data/processed/mgm/c3_su/"
dir.create(out_folder)

comb_str_q <- expand_grid(stratum = c("UM", "HM", "SA"),
                          quality = c("31", "33", "51", "55"))

interval_intensity <- tribble(
  ~stratum, ~quality, ~interval, ~intensity,
      "UM",     "31",      "25",     "0.15",
      "UM",     "33",      "20",     "0.15",
      "UM",     "51",      "25",     "0.15",
      "UM",     "55",      "20",      "0.2",
      "HM",     "31",      "35",     "0.25",
      "HM",     "33",      "30",      "0.3",
      "HM",     "51",      "35",     "0.25",
      "HM",     "55",      "25",      "0.3",
      "SA",     "31",      "45",     "0.25",
      "SA",     "33",      "40",      "0.3",
      "SA",     "51",      "45",     "0.25",
      "SA",     "55",      "40",      "0.3"
  )


min_spc_shares <- tribble(
  ~stratum, ~aalb, ~apse, ~fsyl, ~pabi,
      "UM", "0.3", "0.1", "0.4",   "0",
      "HM", "0.5",   "0",   "0", "0.3",
      "SA",   "0",   "0",   "0",   "0"
  )

combinations <- comb_str_q %>% 
  left_join(interval_intensity, by = c("stratum", "quality")) %>% 
  left_join(min_spc_shares, by = "stratum")

# read template -----------------------------------------------------------
mgm_template <- read_delim(template_file,
                          delim = ";",
                          col_types = cols())


# create mgm files --------------------------------------------------------
for (i in 1:nrow(combinations)) {
  mgm_new <- mgm_template
  
  # values to use
  vals <- combinations[i,] %>% 
    mutate(across(everything(), as.character))
  
  # new file name
  filename_new <- str_c("mgm_c3_su_eq_mgm_",
                        pull(vals, stratum), "_",
                        pull(vals, quality), ".csv")
  
  # start time
  mgm_new[[1, 2]] <- "10"
  
  # interval
  mgm_new[[2, 2]] <- pull(vals, interval)
  
  # intensity
  mgm_new[[3, 2]] <- pull(vals, intensity)
  
  # min D
  mgm_new[[4, 2]] <- "12"
  
  # min species shares
  mgm_new[[5, 2]] <- pull(vals, aalb)
  mgm_new[[6, 2]] <- pull(vals, apse)
  mgm_new[[7, 2]] <- pull(vals, fsyl)
  mgm_new[[8, 2]] <- pull(vals, pabi)
  
  # aggregation
  mgm_new[[9, 2]] <- "3"
  
  # buffer
  mgm_new[[10, 2]] <- "2"
  
  # weights
  mgm_new[[11, 2]] <- "10" # spc
  mgm_new[[12, 2]] <- "100" # BA
  mgm_new[[13, 2]] <- "20" # CR
  mgm_new[[14, 2]] <- "30" # reg_gr
  mgm_new[[15, 2]] <- "20" # reg_nb
  
  # save mgm file
  write_delim(x     = mgm_new,
              file  = str_c(out_folder, filename_new),
              delim = ";")
}

