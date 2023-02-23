####
## Generate bare-ground init-Data
## 25.1.23, us
###


# setup -------------------------------------------------------------------
strata <- c("UM", "HM", "SA")
rows <- c(10, 14, 16)
cols <- rows

data_out_location <- "c3_BG"

library(tidyverse)
source("code/prepinit/functions_init.R")

dir.create(str_c("data/processed/init/", data_out_location))

# loop over strata --------------------------------------------------------
for (i in seq_along(strata)) {
  if (strata[i] %in% c("SM", "UM")) {
    cellsize_m2 <- 100
    cohorts_per_cell <- 2
  } else if (strata[i] == "HM") {
    cellsize_m2 <- 52.02
    cohorts_per_cell <- 1
  } else if (strata[i] == "SA") {
    cellsize_m2 <- 39.06
    cohorts_per_cell <- 1
  }
  
  grid_settings <- list(nrow = rows[i],
                        ncol = cols[i])
  
  
  # check input -------------------------------------------------------------
  if (grid_settings$ncol %% 2 != 0) {
    cat("---ERROR---\nncol has to be an even number!\n-----------")
    quit(save = "ask")
  }
  
  
  # update settings ---------------------------------------------------------
  grid_settings$ncell <- with(grid_settings, nrow * ncol)
  grid_settings$ncohorts_per_cell <- cohorts_per_cell
  grid_settings$stratum <- strata[i]
  grid_settings$cellsize_m2 <- cellsize_m2
  
  grid_setings_table <- bind_rows(grid_settings) %>%
    mutate(across(everything(), as.character)) %>% 
    pivot_longer(everything(), names_to = "var", values_to = "val")
  
  
  # create init-file --------------------------------------------------------
  init_cells <- tibble(cell_ID    = c(1:grid_settings$ncell),
                       Species    = "none",
                       H          = 0,
                       HCB        = 0,
                       D          = 0,
                       N          = 0)
  init_cohorts <- tibble(cell_ID   = rep(c(1:grid_settings$ncell),
                                         each = grid_settings$ncohorts_per_cell),
                         cohort_Nr = rep(c(1:grid_settings$ncohorts_per_cell),
                                         times = grid_settings$ncell))
  init_bg <- left_join(init_cells, init_cohorts, by = "cell_ID") %>% 
    select(cell_ID, cohort_Nr, everything())
  
  
  # save --------------------------------------------------------------------
  write_csv(init_bg,
            str_c("data/processed/init/", data_out_location, "/init_", strata[i], "_BG_", rows[i], "-", cols[i], ".csv"))
  
  f.write_descr(type       = "BG",
                str        = strata[i],
                name       = str_c(rows[i], "-", cols[i]),
                sim_t      = grid_setings_table,
                folder_out = data_out_location)
  
}

