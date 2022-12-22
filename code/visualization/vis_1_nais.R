#####
# visualize nais assessment
# one simulation
# 7.6.22, us
#####

# nais_assessment <- "EFM_UM_Buttes_BDP_reg0_precal_LED"

PROFORM_vis_1_nais <- function(nais_assessment, avg_across, out_subfolder) {
  # setup -------------------------------------------------------------------
  suppressWarnings(suppressMessages(library(tidyverse)))
  
  
  # load data ---------------------------------------------------------------
  nais_indices_red <- read_rds(str_c("data/processed/naisoutput/", nais_assessment, "/nais_indices_red.rds"))
  
  
  # summarise across years --------------------------------------------------
  if(!is.na(avg_across)) {
    nais_indices_red_gr <- nais_indices_red %>% 
      mutate(ts_group = cut_width(time_Step,
                                  width = avg_across,
                                  boundary = min(time_Step) - 1)) %>% 
      group_by(ts_group) %>% 
      mutate(ts_group_min = min(time_Step),
             ts_group_max = max(time_Step),
             ts_group_mean = (ts_group_min + ts_group_max) / 2)
    
    ts_means <- nais_indices_red_gr %>% 
      select(simulation:rockfall_scenario, ts_group:ts_group_mean) %>% 
      distinct() %>% 
      rename(time_Step = "ts_group_mean")
    
    nais_indices_red <- nais_indices_red_gr %>% 
      summarise(across(mix:sapthi, mean, na.rm = TRUE),
                .groups = "drop") %>% 
      left_join(ts_means, by = c("ts_group")) %>% 
      select(-starts_with("ts_gr"))
  }
  
  
  # plot --------------------------------------------------------------------
  gg <- nais_indices_red %>%
    pivot_longer(cols = -c(simulation, natural_hazard, rockfall_scenario, time_Step),
                 names_to = "index",
                 values_to = "value") %>%
    mutate(index = factor(index, levels = c("mix", "vert", "horiz", "supptr", "seedl", "sapthi"))) %>%
    ggplot(aes(time_Step, value)) +
    geom_line() +
    geom_point() +
    facet_wrap(~index, ncol = 1) +
    scale_x_continuous(name = "time [y]") +
    scale_y_continuous(name = "",
                       limits = c(-1, 1),
                       breaks = c(-1, 0, 1),
                       labels = c("very bad", "minimal", "ideal")) +
    coord_flip() +
    labs(title    = "NaiS form 2",
         subtitle = nais_assessment) +
    theme_bw() +
    theme(panel.grid.minor = element_blank())
  
  if (!is.na(avg_across)) {
    gg <- gg +
      labs(caption = str_c("Average across ", avg_across, " years"))
  }
  

  # save --------------------------------------------------------------------
  out_folder <- "results/nais_vis/1_sim/"
  if (!is.na(out_subfolder)){
    out_folder <- str_c(out_folder, out_subfolder, "/")
    dir.create(out_folder, showWarnings = FALSE)
  }
  
  if (is.na(avg_across)) {
    fname <- str_c(out_folder, "F2_", nais_assessment, ".png")
  } else {
    fname <- str_c(out_folder, "F2_", nais_assessment, "_avg", avg_across, ".png")
  }
  
  
  suppressWarnings(ggsave(filename = fname,
                          width = 5,
                          height = 15,
                          units = "cm",
                          scale = 2))
}

