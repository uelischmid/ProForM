## create simsettings for c3_ST_aux simulations
## 27.1.23, us


# setup -------------------------------------------------------------------
library(tidyverse)

template_file <- "data/processed/simsettings/0_simsettings_template.csv"
out_folder <- "data/processed/simsettings/c3_ST_aux/"
dir.create(out_folder)

comb_str_q_init <- expand_grid(stratum = c("UM", "HM", "SA"),
                               quality = c("31", "33", "51", "55"),
                               init    = c(1:3))

grid_dim <- tibble(stratum  = c("UM", "HM", "SA"),
                   grid_dim = c(10, 14, 16))

spc_shares <- tribble(
  ~stratum, ~quality, ~aalb, ~apse, ~fsyl, ~pabi,
      "UM",     "31",    0L,    0L,   80L,   20L,
      "UM",     "33",   30L,   15L,   50L,    5L,
      "UM",     "51",    0L,    0L,   80L,   20L,
      "UM",     "55",   30L,   15L,   50L,    5L,
      "HM",     "31",    0L,    0L,    0L,  100L,
      "HM",     "33",   60L,    0L,    0L,   40L,
      "HM",     "51",    0L,    0L,    0L,  100L,
      "HM",     "55",   60L,    0L,    0L,   40L,
      "SA",     "31",    0L,    0L,    0L,  100L,
      "SA",     "33",    0L,    0L,    0L,  100L,
      "SA",     "51",    0L,    0L,    0L,  100L,
      "SA",     "55",    0L,    0L,    0L,  100L
  )

combinations <- comb_str_q_init %>% 
  left_join(grid_dim, by = "stratum") %>% 
  left_join(spc_shares, by = c("stratum", "quality"))


# read template -----------------------------------------------------------
ss_template <- read_delim(template_file,
                          delim = ";",
                          col_types = cols())


# create simsettings ------------------------------------------------------
for (i in 1:nrow(combinations)) {
  ss_new <- ss_template
  
  # values to use
  vals <- combinations[i,] %>% 
    mutate(across(everything(), as.character))
  
  
  # new file name
  filename_new <- str_c("simsettings_c3_ST_aux_",
                        pull(vals, stratum), "_",
                        pull(vals, quality), "_",
                        "init", pull(vals, init), ".csv")
  
  # sim name
  ss_new[[1, 2]] <- str_c("c3_ST_aux_",
                          pull(vals, stratum), "_",
                          pull(vals, quality), "_",
                          "init", pull(vals, init))
  
  # sim_init
  ss_new[[2, 2]] <- str_c("c3_init/init_ST",
                          pull(vals, init), "_",
                          pull(vals, stratum), "_",
                          pull(vals, quality), ".csv")
  
  # sim_param
  ss_new[[3, 2]] <- "paramfordyn_cal.csv"
  
  # sim_time
  ss_new[[4, 2]] <- "100"
  
  # sim_seed
  ss_new[[5, 2]] <- "1"
  
  # sim_stratum
  ss_new[[6, 2]] <- pull(vals, stratum)
  
  # grid dimensions
  ss_new[[7, 2]] <- pull(vals, grid_dim)
  ss_new[[8, 2]] <- pull(vals, grid_dim)
  
  # aspect & slope
  ss_new[[9, 2]] <- "235"
  ss_new[[10, 2]] <- "36"
  
  # qualities
  ss_new[[11, 2]] <- str_sub(pull(vals, quality), end = 1)
  ss_new[[12, 2]] <- str_sub(pull(vals, quality), start = 2)
  
  # species shares
  ss_new[[13, 2]] <- pull(vals, aalb)
  ss_new[[14, 2]] <- pull(vals, apse)
  ss_new[[15, 2]] <- pull(vals, fsyl)
  ss_new[[16, 2]] <- pull(vals, pabi)
  ss_new[[17, 2]] <- "50"
  
  # mgm
  ss_new[[18, 2]] <- "none"
  ss_new[[19, 2]] <- "NA"
  
  # save simsettings
  write_delim(x     = ss_new,
              file  = str_c(out_folder, filename_new),
              delim = ";")
}
