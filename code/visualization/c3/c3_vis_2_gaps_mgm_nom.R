### visualize gaps
### mgm and corresponding nom-simulation
## 2.1.24, us


PROFORM_vis_2_gaps_c3 <- function(simulation_mgm, plot_timesteps, out_subfolder) {
  suppressWarnings(suppressMessages(library(tidyverse)))
  
  # get NOM-simulation and combination name ---------------------------------
  simulation_nom <- simulation_mgm %>% 
    str_split("_") %>% 
    magrittr::extract2(1)
  
  if (simulation_nom[2] == "ST") {
    simulation_nom[7] <- "NOM"
    simulation_nom[8] <- "t0"
    simulation_nom[9] <- "i0"
  } else if (simulation_nom[2] == "LT") {
    simulation_nom[5] <- "NOM"
    simulation_nom[6] <- "t0"
    simulation_nom[7] <- "i0"
  } else {
    cat("ERROR: only c3 simulations allowed)")
    quit(save = "ask")
  }
  
  simulation_nom <- str_c(simulation_nom, collapse = "_")
  
  # combination name
  comb_name <- simulation_mgm %>% 
    str_replace("c3_", "") %>% 
    str_replace("def_", "")
  
  out_folder <- str_c("results/nais_vis/gaps/", out_subfolder, "/")
  dir.create(out_folder, recursive = TRUE, showWarnings = FALSE)
  
  # load data ---------------------------------------------------------------
  coordinates <- read_rds(str_c("data/processed/simoutput/", simulation_mgm, "/3_output/coordinates.rds"))
  
  gap_data <- vector(mode = "list", length = 2)
  gap_data[[1]] <- read_rds(str_c("data/processed/naisoutput/1_prep/", simulation_mgm, "/gap_data.rds")) %>% 
    filter(time_Step %in% plot_timesteps) %>% 
    mutate(simulation = "mgm")
  gap_data[[2]] <- read_rds(str_c("data/processed/naisoutput/1_prep/", simulation_nom, "/gap_data.rds")) %>% 
    filter(time_Step %in% plot_timesteps) %>% 
    mutate(simulation = "NOM")
  gap_data <- bind_rows(gap_data) %>% 
    mutate(simulation = factor(simulation, levels = c("mgm", "NOM"))) %>% 
    unnest(cc_IDs)
  
  
  coordinates_full <- bind_rows(replicate(2 * length(plot_timesteps), coordinates, simplify = FALSE)) %>% 
    bind_cols(tibble(simulation = rep(c("mgm", "NOM"), each = nrow(coordinates) * length(plot_timesteps)),
                     time_Step  = rep(rep(plot_timesteps, each = nrow(coordinates)), times = 2)) %>% 
                mutate(simulation = factor(simulation, levels = c("mgm", "NOM"))))
  
  
  coor_gaps <- left_join(coordinates_full, gap_data, by = c("simulation" = "simulation",
                                                            "time_Step" = "time_Step",
                                                            "cell_ID" = "cc_IDs"))
  gg_p <- ggplot(coor_gaps, aes(corner_x, corner_y, group = cell_ID, fill = area_m2/100)) +
    geom_polygon(color = "black", size = 0.1) +
    coord_equal() +
    facet_grid(rows = vars(time_Step),
               cols = vars(simulation)) +
    scale_fill_continuous(type      = "viridis",
                          direction = -1,
                          na.value  = "white") +
    labs(title = "Canopy Gaps",
         caption = comb_name,
         x = "x (m)",
         y = "y (m)",
         fill = "Size (a)") +
    theme_bw() +
    theme(panel.grid.minor = element_blank())
  
  ggsave(filename = str_c(out_folder, "Gaps_", comb_name, ".jpg"),
         plot = gg_p,
         width = 5,
         height = 10,
         scale = 1)
} 




