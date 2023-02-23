## create simsettings for c3_LT simulations
## 4.2.23, us


# setup -------------------------------------------------------------------
library(tidyverse)

template_file <- "data/processed/simsettings/0_simsettings_template.csv"
out_folder <- "data/processed/simsettings/c3_LT/"
dir.create(out_folder)

combinations_mgm <- expand_grid(stratum           = c("UM", "HM", "SA"),
                                quality           = c("31", "33", "51", "55"),
                                sim_time          = 1000,
                                sim_seed          = 1,
                                sim_aspect        = 235,
                                sim_slope         = 36,
                                spc_pressure      = 50,
                                mgm_type1         = c("STS", "GRS1", "GRS2", "CAB1", "CAB2", "SC"),
                                mgm_time          = seq(10, 40, 10),
                                mgm_intensity     = seq(10, 40, 10))


combinations_nom <- expand_grid(stratum           = c("UM", "HM", "SA"),
                                quality           = c("31", "33", "51", "55"),
                                sim_time          = 1000,
                                sim_seed          = 1,
                                sim_aspect        = 235,
                                sim_slope         = 36,
                                spc_pressure      = 50,
                                mgm_type1         = "NOM",
                                mgm_time          = 0,
                                mgm_intensity     = 0)

combinations <- bind_rows(combinations_mgm, combinations_nom) %>% 
  mutate(mgm_type2 = case_when(mgm_type1 == "STS"               ~ "single_tree_selection",
                               mgm_type1 %in% c("GRS1", "GRS2") ~ "group_selection",
                               mgm_type1 %in% c("CAB1", "CAB2") ~ "cableyarding",
                               mgm_type1 == "SC"                ~ "slit_cuts",
                               TRUE                             ~ "none"))

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

combinations <- combinations %>% 
  left_join(grid_dim, by = "stratum") %>% 
  left_join(spc_shares, by = c("stratum", "quality"))

# combinations <- split(combinations,
#                       rep(1:3,
#                           length.out = nrow(combinations),
#                           each       = ceiling(nrow(combinations) / 3)))
# combinations <- combinations[[3]]


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
  filename_new <- str_c("simsettings_c3_LT_",
                        pull(vals, stratum), "_",
                        pull(vals, quality), "_",
                        pull(vals, mgm_type1), "_",
                        "t", pull(vals, mgm_time), "_",
                        "i", pull(vals, mgm_intensity),
                        ".csv")
  
  # sim name
  ss_new[[1, 2]] <- str_c("c3_LT_",
                          pull(vals, stratum), "_",
                          pull(vals, quality), "_",
                          pull(vals, mgm_type1), "_",
                          "t", pull(vals, mgm_time), "_",
                          "i", pull(vals, mgm_intensity))
  
  # sim_init
  ss_new[[2, 2]] <- str_c("c3_init/init_LT_",
                          pull(vals, stratum), "_",
                          pull(vals, quality), ".csv")
  
  # sim_param
  ss_new[[3, 2]] <- "paramfordyn_cal.csv"
  
  # sim_time
  ss_new[[4, 2]] <- pull(vals, sim_time)
  
  # sim_seed
  ss_new[[5, 2]] <- pull(vals, sim_seed)
  
  # sim_stratum
  ss_new[[6, 2]] <- pull(vals, stratum)
  
  # grid dimensions
  ss_new[[7, 2]] <- pull(vals, grid_dim)
  ss_new[[8, 2]] <- pull(vals, grid_dim)
  
  # aspect & slope
  ss_new[[9, 2]] <- pull(vals, sim_aspect)
  ss_new[[10, 2]] <- pull(vals, sim_slope)
  
  # qualities
  ss_new[[11, 2]] <- str_sub(pull(vals, quality), end = 1)
  ss_new[[12, 2]] <- str_sub(pull(vals, quality), start = 2)
  
  # species shares
  ss_new[[13, 2]] <- pull(vals, aalb)
  ss_new[[14, 2]] <- pull(vals, apse)
  ss_new[[15, 2]] <- pull(vals, fsyl)
  ss_new[[16, 2]] <- pull(vals, pabi)
  ss_new[[17, 2]] <- pull(vals, spc_pressure)
  
  # mgm
  ss_new[[18, 2]] <- pull(vals, mgm_type2)
  if (pull(vals, mgm_type2) == "none") {
    ss_new[[19, 2]] <- "NA"
  } else {
    ss_new[[19, 2]] <- str_c("c3_mgm/mgm_",
                             pull(vals, mgm_type1), "_",
                             pull(vals, stratum), "_",
                             "t", pull(vals, mgm_time), "_",
                             "i", pull(vals, mgm_intensity), ".csv")
  }
  
  # save simsettings
  write_delim(x     = ss_new,
              file  = str_c(out_folder, filename_new),
              delim = ";")
}
